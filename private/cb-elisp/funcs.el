;;; Define intelligent M-RET command.

(defconst elisp/let-expression-re
  (regexp-opt '("(let" "(-if-let*" "(-when-let*"))
  "Regex matching the start of a let expression.")

(defun elisp/let-expr-start ()
  "Move to the start of a let expression."
  (cl-flet ((at-let? () (thing-at-point-looking-at elisp/let-expression-re)))
    (while (and (sp-backward-up-sexp) (not (at-let?))))
    (when (at-let?) (point))))

(defun elisp/at-let-binding-form? ()
  "Non-nil if point is at the top of a binding form in a let expression."
  (and (save-excursion (elisp/let-expr-start))
       (save-excursion
         (sp-backward-up-sexp 3)
         (thing-at-point-looking-at elisp/let-expression-re))))

(defun elisp/M-RET ()
  "Perform context-sensitive newline behaviour."
  (interactive)
  (cond
   ;; Insert let-binding
   ((save-excursion (elisp/let-expr-start))
    (until (elisp/at-let-binding-form?) (sp-backward-up-sexp))
    (sp-up-sexp)
    (newline-and-indent)
    (insert "()")
    (forward-char -1))
   (t
    (sp-up-sexp)
    (newline-and-indent)
    (when evil-mode
      (evil-insert-state)))))

;;; C-c C-c eval command

(defun elisp/thing-for-eval ()
  (cond
   ((region-active-p)
    (call-interactively 'eval-region)
    (list :beg (region-beginning)
          :end (region-end)))

   ((thing-at-point 'defun)
    (save-excursion
      (beginning-of-defun)
      (call-interactively 'eval-defun)
      (list :beg (point)
            :end (save-excursion (end-of-defun) (point)))))

   ((ignore-errors (preceding-sexp))
    (call-interactively 'eval-last-sexp)
    (cl-destructuring-bind (&optional beg . end)
        (when (and beg end)
          (save-excursion
            (backward-char)
            (bounds-of-thing-at-point 'sexp))
          (list :beg beg :end end))))))

(defun elisp/eval-dwim ()
  "Perform a context-sensitive eval command.
Return the bounds of the evaluated form."
  (interactive)
  (-if-let (thing (elisp/thing-for-eval))
      (cl-destructuring-bind (&key beg end &allow-other-keys) thing
        (eval-region beg end))
    (user-error "Nothing to evaluate")))

