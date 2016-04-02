;;; funcs.el --- Helper functions for cb-smartparens layer.
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'thingatpt)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")
(autoload 'smart-ops-backspace "smart-ops")
(autoload 'smartparens-mode "smartparens")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'sp-get-pair "smartparens")
(autoload 'sp-in-code-p "smartparens")
(autoload 'sp-in-string-p "smartparens")
(autoload 'sp-up-sexp "smartparens")
(autoload 'sp-split-braced-expression-over-new-lines "sp-generic-prog")


;;; Utility commands

(defun sp-external-padding (id action ctx)
  "Add external padding around ID.
Insert leading padding unless at start of line or after an open round paren."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (save-excursion
      (when (search-backward (sp-get-pair id :open)
                             (line-beginning-position) t)
        (let ((bol-to-point (buffer-substring (line-beginning-position) (point))))
          (cond
           ((s-matches? (rx bol (* space) eol) bol-to-point))
           ((s-matches? (rx (or "(" "[") (* space) eol) bol-to-point)
            (delete-horizontal-space))
           (t
            (just-one-space))))
        t))))

(defun sp-internal-padding (id action ctx)
  "Add internal padding around ID."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (when (search-backward (sp-get-pair id :open)
                           (line-beginning-position) t)

      (goto-char (match-end 0))
      (insert "  ")
      (forward-char -1))))

(defun sp-internal-and-external-padding (id action context)
  "Insert internal and external padding."
  (and (sp-external-padding id action context)
       (sp-internal-padding id action context)))

(defun sp-just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))


;;; OCaml utils

(defun sp/ml-just-one-space (id action ctx)
  "Pad delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (sp-just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is the first position of another list
    ;; 2. this form begins a new line.
    ;; 3. this form is preceded by a `?`, as in a let binding.
    ;; 4. this form is preceded by a `:`, as in a keyword argument
    ;; 5. this form is preceded by a `.`, as in an array index expression
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "." "," ":" "(" "[" "[|" "{" "?")
                       ;; HACK: utop prompt
                       (and "utop[" (+ digit) "]" ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (s-matches? (rx (or (any ")" "]" "|]" "}") eol))
                          (char-to-string (char-after)))
        (just-one-space)))))


;;; Lisp utils

(defun sp/lisp-just-one-space (id action ctx)
  "Pad Lisp delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (sp-just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is a quoted form
    ;; 2. this is the first position of another list
    ;; 3. this form begins a new line.
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "," "`" "'" "@" "#" "~" "(" "[" "{")
                       ;; HACK: nREPL prompt
                       (and (any alnum "." "/" "-") ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (-contains? '(")" "]" "}") (char-to-string (char-after)))
        (just-one-space)))))


;;; C utils

(defun sp/c++-format-after-open-curly (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (thing-at-point-looking-at
              (rx (or "}" ")" "else" "const" "override") (* space) "{")))
    (save-excursion
      (search-backward "{")
      (just-one-space))
    (sp-split-braced-expression-over-new-lines ";")))

(defun sp/c-format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (thing-at-point-looking-at
              (rx symbol-start (or "=" "return" "if" "while" "for")
                  (* space) "(")))
    (save-excursion
      (search-backward "(")
      (just-one-space))))


;;; Ruby utils

(defun sp/ruby-should-insert-pipe-close (_id _action _ctx)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair."
  (thing-at-point-looking-at
   (rx-to-string `(and (or "do" "{") (* space) "|"))))

(defun sp/ruby-sp-hook-space-before (_id action _ctx)
  "Move to point before ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-backward "|")
      (just-one-space))))

(defun sp/ruby-sp-hook-space-after (_id action _ctx)
  "Move to point after ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-forward "|")
      (just-one-space))))


;;; Scala utils

(defun sp/scala-format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               (search-backward "(")
               (thing-at-point-looking-at
                (rx symbol-start (or "=" "return" "if" "while" "for" "case")
                    (* space)))))
    (save-excursion
      (search-backward "(")
      (just-one-space))))

(defun sp/scala-curly-brace-padding (id action context)
  "Insert internal and external padding."
  (if (s-matches? (rx bol (* space) "import" eow) (cb-buffers-current-line))
      (sp-internal-padding id action context)
    (and (sp-external-padding id action context)
         (sp-internal-padding id action context))))


;;; Haskell utils

(defun sp/haskell-external-padding (id action ctx)
  "Add external padding around ID.
Insert leading padding unless at start of line or after an open round paren."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (save-excursion
      (when (search-backward (sp-get-pair id :open)
                             (line-beginning-position) t)
        (let ((bol-to-point (buffer-substring (line-beginning-position) (point))))
          (cond
           ((s-matches? (rx bol (* space) eol) bol-to-point))
           ((s-matches? (rx (or "@" "(" "[") (* space) eol) bol-to-point)
            (delete-horizontal-space))
           (t
            (just-one-space))))
        t))))

;;; funcs.el ends here
