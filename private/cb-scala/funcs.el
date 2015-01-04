;;; Smart ops

(require 's)
(require 'dash)

(defun scala/equals ()
  (interactive)
  (super-smart-ops-insert "="))

(defun scala/colon ()
  (interactive)
  (core/insert-smart-op-no-leading-space ":"))

(defun scala/repl-colon ()
  (interactive)
  (if (s-matches? (rx bol (* space) "scala>" (* space))
                  (buffer-substring (line-beginning-position) (point)))
      (insert ":")
    (core/insert-smart-op-no-leading-space ":")))

(defun scala/insert-variance-op (op)
  "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
  (cond
   ;; No padding at the start of type parameter.
   ((thing-at-point-looking-at (rx "[" (* space)))
    (delete-horizontal-space)
    (insert op))
   ;; Leading padding after a comma, e.g. for a type parameter or function call.
   ((thing-at-point-looking-at (rx "," (* space)))
    (just-one-space)
    (insert op))
   ;; Otherwise leading and trailing padding.
   (t
    (super-smart-ops-insert op))))

(defun scala/minus ()
  (interactive "*")
  (scala/insert-variance-op "-"))

(defun scala/plus ()
  (interactive "*")
  (scala/insert-variance-op "+"))

(defun scala/slash ()
  "Insert a slash as a smart op.
Typing three in a row will insert a ScalaDoc."
  (interactive "*")
  (if (s-matches? (rx bol (* space) "//" (* space) eol) (current-line))
      (atomic-change-group
        (delete-region (line-beginning-position) (line-end-position))
        (scala/insert-scaladoc))
    (super-smart-ops-insert "/")))

(defun scala/insert-scaladoc ()
  "Insert the skeleton of a ScalaDoc at point."
  (interactive "*")
  (indent-for-tab-command) (insert "/** ")
  (save-excursion
    (newline) (indent-for-tab-command) (insert "*/")))


;;; Interactive

(defun scala/switch-to-src ()
  "Switch back to the last scala source file."
  (interactive)
  (-when-let (buf (car (--filter-buffers (derived-mode-p 'scala-mode))))
    (pop-to-buffer buf)))

(defun scala/load-buffer (file)
  "Load FILE, starting an inferior scala if needed."
  (interactive (list (buffer-file-name)))
  (unless (get-buffer ensime-inf-buffer-name)
    (ensime-inf-run-scala))
  (ensime-inf-load-file file))


;;; Smart editing commands

(defun scala/after-open-curly? ()
  (s-matches? (rx "{" (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/before-close-curly? ()
  (s-matches? (rx bos (* space) "}")
              (buffer-substring (point) (line-end-position))))

(defun scala/between-empty-curly-braces? ()
  (and (scala/after-open-curly?) (scala/before-close-curly?)))

(defun scala/between-curly-braces-with-content? ()
  (or (and (scala/after-open-curly?)
           (s-matches? (rx (+ (not space)) (* space) "}")
                       (buffer-substring (point) (line-end-position))))
      (and (scala/before-close-curly?)
           (s-matches? (rx "{" (* space) (+ (not space)))
                       (buffer-substring (line-beginning-position) (point))))))

(defun scala/split-braced-expression-over-new-lines ()
  "Split the braced expression on the current line over several lines."
  (-let [(&plist :beg beg :end end) (sp-get-enclosing-sexp)]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (newline-and-indent)
      (while (search-forward ";" (line-end-position) t)
        (replace-match "\n")))))

(defun scala/after-lambda-arrow? ()
  (s-matches? (rx (* space) "=>" (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/expand-brace-group-for-hanging-lambda ()
  (scala/split-braced-expression-over-new-lines)
  (goto-char (plist-get (sp-get-enclosing-sexp) :beg))
  (scala/join-line)
  (goto-char (line-end-position))
  (newline-and-indent))

(defun scala/ret (&optional arg)
  "Insert a newline with context-sensitive formatting."
  (interactive "P")
  (cond
   ((scala/at-scaladoc?)
    (goto-char (line-end-position))
    (newline)
    (indent-for-tab-command)
    (insert "* "))

   ((or arg (core/in-string-or-comment?))
    (comment-indent-new-line)
    (just-one-space))

   ((scala/between-empty-curly-braces?)
    (scala/split-braced-expression-over-new-lines)
    (forward-line)
    (indent-for-tab-command))

   ((and (scala/after-lambda-arrow?) (scala/between-curly-braces-with-content?))
    (scala/expand-brace-group-for-hanging-lambda))

   ((scala/between-curly-braces-with-content?)
    (delete-horizontal-space)
    (scala/split-braced-expression-over-new-lines)
    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (scala/after-open-curly?)
      (forward-line)
      (back-to-indentation)))
   (t
    (call-interactively 'comment-indent-new-line))))

(defun scala/at-scaladoc? ()
  (s-matches? (rx bol (* space) (? "/") (+ "*")) (current-line)))

(defun scala/at-case-class? ()
  (s-matches? (rx bol (* space) "case" (+ space) "class" eow) (current-line)))

(defun scala/at-abstract-sealed-class? ()
  (s-matches? (rx bol (* space) "abstract" (+ space) "sealed" (+ space) "class" eow) (current-line)))

(defun scala/meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- In match statements
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "var" eow) (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "var" (yas--template-name sn))))
    (message "New var binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "val" eow) (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "val" (yas--template-name sn))))
    (message "New val binding"))

   ;; Insert new case class.
   ((or (scala/at-case-class?) (scala/at-abstract-sealed-class?))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case class" (yas--template-name sn))))
    (message "New case class"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "case") (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case" (yas--template-name sn))))
    (message "New data case"))

   ;; Insert new import statement
   ((s-matches? (rx bol (* space) "import" eow) (current-line))
    (core/open-line-below-current-indentation)
    (insert "import ")
    (message "New import statement"))

   (t
    (goto-char (line-end-position))
    (scala/ret)))

  (evil-insert-state))

(defun scala/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "(") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" ")"))
              (buffer-substring (point) (line-end-position))))

(defun scala/smart-space ()
  "Insert a space, performing extra padding inside lists."
  (interactive)
  (cond
   ((and (scala/after-subexpr-opening?) (scala/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun scala/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((and (scala/after-subexpr-opening?)
          (scala/before-subexp-closing?)
          (thing-at-point-looking-at (rx (+ space))))
     (delete-horizontal-space))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))


;;; File template

(defun scala/package-for-current-file ()
  (-if-let* ((root (and (buffer-file-name) (projectile-project-p)))
             (pkg-id (scala/filepath-to-package-name root (buffer-file-name))))
      (if (s-blank? pkg-id) "" (format "package %s" pkg-id))
    ""))

(defun scala/filepath-to-package-name (root file-name)
  (->> (f-dirname file-name)
       (s-chop-prefix root)
       f-split
       nreverse
       (--take-while (not (or (equal "src" it) (equal "scala" it))))
       nreverse
       (s-join ".")))
