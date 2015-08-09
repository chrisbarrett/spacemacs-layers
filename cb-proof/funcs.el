(defun coq/configure-coq-buffer ()
  "Set buffer local variables and hooks."
  (setq-local compile-command (concat "coqc " (buffer-name))))


;;; Smart M-RET

(defun coq/case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun coq/newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (if (search-forward "." (line-end-position) t)
      (forward-char -1)
    (goto-char (line-end-position)))

  (newline)
  (indent-to col)
  (insert str))

(defun coq/newline-and-expand-snippet-at-col (predicate col)
  "Insert a new line, find the template matching PREDICATE and insert at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (yas-insert-first-snippet predicate))

(defun coq/rx-start-column (rx)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp rx (line-beginning-position))
    (current-column)))

(defun coq/meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond

   ;; Insert case after match statement.

   ((s-matches? (rx symbol-start "match" symbol-end) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (goto-char (max (line-beginning-position) (- (point) 2)))
    (yas-insert-first-snippet (lambda (sn)
                                (equal "match-case" (yas--template-name sn)))))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) (? "Co") "Inductive") (current-line))
    (coq/newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert dependent type below the current one.
   ((s-matches? (rx bol (* space) "|" (* space) (+ word) (+ space) ":" (+ space))
                (current-line))
    (coq/newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "=>") (current-line))
    (coq/newline-and-expand-snippet-at-col
     (lambda (sn)
       (equal "match-case" (yas--template-name sn)))
     (coq/case-start-col)))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (coq/newline-and-insert-at-col (coq/case-start-col) "| "))

   ;; Insert check.
   ((s-matches? (rx bol "Check") (current-line))
    (coq/newline-and-expand-snippet-at-col
     (lambda (sn)
       (equal "Check" (yas--template-name sn)))
     (coq/rx-start-column "Check")))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (evil-insert-state))
