;;; funcs.el --- Helper functions for cb-yasnippet layer.
;;; Commentary:
;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)
(require 'thingatpt)

(autoload 'yas--all-templates "yasnippet")
(autoload 'yas--field-contains-point-p "yasnippet")
(autoload 'yas--field-end "yasnippet")
(autoload 'yas--field-modified-p "yasnippet")
(autoload 'yas--field-start "yasnippet")
(autoload 'yas--field-text-for-display "yasnippet")
(autoload 'yas--get-snippet-tables "yasnippet")
(autoload 'yas--skip-and-clear "yasnippet")
(autoload 'yas--template-content "yasnippet")
(autoload 'yas--template-expand-env "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")
(autoload 'yas-next-field "yasnippet")
(autoload 'yas-reload-all "yasnippet")

(defvar yas-text nil)
(defvar yas-snippet-dirs nil)

;;; Snippet dir registration

(defvar cb-yasnippet/yas-dirs nil)

(defun cb-yas/register-snippets-dir (dir)
  (let ((updated (-uniq (-cons* cb-yasnippet/main-snippets-dir dir cb-yasnippet/yas-dirs))))
    (setq cb-yasnippet/yas-dirs updated))
  (cb-yas/reload-all))

(defun cb-yas/reload-all ()
  (interactive)
  (setq yas-snippet-dirs cb-yasnippet/yas-dirs)
  (yas-reload-all))

(defun cb-yas/sync-with-yasnippet ()
  (unless (equal cb-yasnippet/yas-dirs yas-snippet-dirs)
    (cb-yas/reload-all)))


;;; Config support

(defmacro yas-with-field-restriction (&rest body)
  "Narrow the buffer to the current active field and execute BODY.
If no field is active, no narrowing will take place."
  (declare (indent 0))
  `(save-restriction
     (when (yas/current-field)
       (narrow-to-region (yas/beginning-of-field) (yas/end-of-field)))
     ,@body))

(defun yas/bol? ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))

(defun yas/msg (fmt &rest args)
  "Like `message', but returns the empty string.
Embed in elisp blocks to trigger messages within snippets."
  (apply 'message (s-prepend "[yas] " fmt) args)
  "")

(defun yas-insert-first-snippet (predicate)
  "Choose a snippet to expand according to PREDICATE."
  (setq yas--condition-cache-timestamp (current-time))
  (let ((yas-buffer-local-condition 'always))
    (-if-let (yas--current-template
              (-first predicate (yas--all-templates (yas--get-snippet-tables))))
        (let ((where (if (region-active-p)
                         (cons (region-beginning) (region-end))
                       (cons (point) (point)))))
          (yas-expand-snippet (yas--template-content yas--current-template)
                              (car where)
                              (cdr where)
                              (yas--template-expand-env yas--current-template)))
      (error "No snippet matching predicate"))))

(defun yas/current-field ()
  "Return the current active field."
  (and (boundp 'yas--active-field-overlay)
       yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (overlay-get yas--active-field-overlay 'yas--field)))

(defun yas/beginning-of-field ()
  (-when-let (field (yas/current-field))
    (marker-position (yas--field-start field))))

(defun yas/end-of-field ()
  (-when-let (field (yas/current-field))
    (marker-position (yas--field-end field))))

(defun yas/current-field-text ()
  "Return the text in the active snippet field."
  (-when-let (field (yas/current-field))
    (yas--field-text-for-display field)))

(defun yas/clear-blank-field (&rest _)
  "Clear the current field if it is blank."
  (-when-let* ((beg (yas/beginning-of-field))
               (end (yas/end-of-field))
               (str (yas/current-field-text)))
    (when (s-matches? (rx bos (+ space) eos) str)
      (delete-region beg end)
      t)))

(defun yas/maybe-goto-field-end ()
  "Move to the end of the current field if it has been modified."
  (-when-let (field (yas/current-field))
    (when (and (yas--field-modified-p field)
               (yas--field-contains-point-p field))
      (goto-char (yas/end-of-field)))))


;;; Elisp

(defun yas/find-identifier-prefix ()
  "Find the commonest identifier prefix in use in this buffer."
  (let ((ns-separators (rx (or ":" "--" "/"))))
    (->> (buffer-string)
         ;; Extract the identifiers from declarations.
         (s-match-strings-all
          (rx bol (* space)
              "(" (? "cl-") (or "defun" "defmacro" "defvar" "defconst")
              (+ space)
              (group (+ (not space)))))
         ;; Find the commonest prefix.
         (-map 'cadr)
         (--filter (s-matches? ns-separators it))
         (--map (car (s-match (rx (group (* nonl) (or ":" "--" "/"))) it)))
         (-group-by 'identity)
         (-max-by (-on '>= 'length))
         (car))))

(defun yas/find-group-for-snippet ()
  "Find the first group defined in the current file,
falling back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not
                                                       space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (f-no-ext (f-filename buffer-file-name))))

(defun yas/simplify-arglist (text)
  "Return a simplified docstring of arglist TEXT."
  (->> (ignore-errors
         (read (format "(%s)" text)))
       (--keep
        (ignore-errors
          (cond
           ((listp it)
            (-first (lambda (x)
                      (and (symbolp x)
                           (not (s-starts-with? "&" (symbol-name x)))))
                    it))
           ((symbolp it) it))))
       (--remove (s-starts-with? "&" (symbol-name it)))))

(defun yas/cl-arglist? (text)
  "Non-nil if TEXT is a Common Lisp arglist."
  (let ((al (ignore-errors (read (format "(%s)" text)))))
    (or (-any? 'listp al)
        (-intersection al '(&key &allow-other-keys &body)))))

(defun yas/defun-form-for-arglist (text)
  "Return either 'defun or 'cl-defun depending on whether TEXT
is a Common Lisp arglist."
  (if (yas/cl-arglist? text) 'cl-defun 'defun))

(defun yas/defmacro-form-for-arglist (text)
  "Return either 'defmacro or 'cl-defmacro depending on whether TEXT
is a Common Lisp arglist."
  (if (yas/cl-arglist? text) 'cl-defmacro 'defmacro))

(defun yas/find-prefix-for-use-package ()
  "Find the name of the package being configured by the name of the enclosing defun."
  (f-filename (f-dirname (buffer-file-name))))

(defun yas/find-ident-for-use-package ()
  "Infer the name of the package being configured by the name of the enclosing defun."
  (save-excursion
    (save-match-data
      (if (search-backward-regexp (rx "defun" (? "*") (+ space) (+ nonl) "/" (? (or "pre-" "post-")) "init-" (group (+ (not space))))
                                  nil t)
          (match-string 1)
        (read-string "Package name: ")))))

(defun yas/autoload-file-for-function (sym)
  (-if-let (file (symbol-file (if (stringp sym) (intern sym) sym)))
      (f-filename (f-no-ext file))
    ""))

(defun yas/at-line-above-decl? ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))


;;; Editing commands

(defun yas/space ()
  "Clear and skip this field if it is unmodified.  Otherwise insert a space."
  (interactive "*")
  (let ((field (yas/current-field))
        (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode)))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          (sp-mode?
           (sp-generic-prog-space))
          (t
           (call-interactively #'self-insert-command)))))

(defun yas/backspace ()
  "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
  (interactive "*")
  (let ((field (yas/current-field))
        (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode)))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((and sp-mode? (derived-mode-p 'prog-mode))
           (sp-generic-prog-backspace))
          (sp-mode?
           (call-interactively #'sp-backward-delete-char))
          (t
           (call-interactively #'backward-delete-char)))))


;;; Rust

(defun yas/rust-bol-or-after-accessibility-modifier? ()
  "Predicate for snippets"
  (save-excursion
    (save-restriction
      ;; Move past access modifier.
      (goto-char (line-beginning-position))
      (search-forward-regexp (rx bow "pub" eow (* space)) (line-end-position) t)
      (narrow-to-region (point) (line-end-position))
      (yas/bol?))))

(defun yas/rust-previous-struct-def ()
  "Search backward for the name of the last struct defined in this file."
  (save-match-data
    (if (search-backward-regexp (rx (or "enum" "struct") (+ space)
                                    (group (+ (not (any ";" "(" "{")))))
                                nil t)
        (s-trim (match-string 1))
      "Name")))



(defun yas/csharp-class-name ()
  "Find the last declared class or struct name in the current file."
  (when (search-backward-regexp (rx "class" (+ space) (group (+ word))) nil t)
    (match-string 1)))


;;; Haskell

(defun yas/haskell-ctor-name (&optional text)
  (car (s-split (rx space) (or text yas-text))))

;;; funcs.el ends here
