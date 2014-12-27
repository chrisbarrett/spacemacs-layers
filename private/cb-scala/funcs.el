;;; Smart ops

(defun scala/equals ()
  (interactive)
  (super-smart-ops-insert "="))

(defun scala/colon ()
  (interactive)
  (core/insert-smart-op-no-leading-space ":"))

(defmacro define-scala-variance-op-command (sym op)
  "Define command named SYM to insert a variance operator OP."
  `(defun ,sym ()
     "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
     (interactive "*")
     (cond
      ;; No padding at the start of type parameter.
      ((thing-at-point-looking-at (rx "[" (* space)))
       (delete-horizontal-space)
       (insert ,op))
      ;; Leading padding after a comma, e.g. for a type parameter or function call.
      ((thing-at-point-looking-at (rx "," (* space)))
       (just-one-space)
       (insert ,op))
      ;; Otherwise leading and trailing padding.
      (t
       (super-smart-ops-insert ,op)))))

(define-scala-variance-op-command scala/plus "+")
(define-scala-variance-op-command scala/minus "-")


;;; Interactive

(defun scala/join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

(defun scala/switch-to-src ()
  "Switch back to the last scala source file."
  (interactive)
  (-when-let (buf (car (--filter-buffers (derived-mode-p 'scala-mode))))
    (pop-to-buffer buf)))


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

(defun scala/ret ()
  "Insert a newline with context-sensitive formatting."
  (interactive)
  (cond
   ((core/in-string-or-comment?)
    (comment-indent-new-line))

   ((scala/between-empty-curly-braces?)
    (scala/split-braced-expression-over-new-lines)
    (forward-line)
    (indent-for-tab-command))

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
   ((s-matches? (rx bol (* space) "case" (+ space) "class" eow) (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case class" (yas--template-name sn))))
    (message "New case class"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "case") (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case" (yas--template-name sn))))
    (message "New data case"))

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


;;; Ensime refactor

(defun scala/ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun scala/ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))
