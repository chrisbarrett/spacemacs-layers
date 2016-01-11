;;; funcs.el --- Helper functions for cb-proof layer.
;;; Commentary:
;;; Code:

(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-insert-state "evil-states")
(autoload 'yas-exit-all-snippets "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")

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

(defun coq/newline-and-indent-to-col (col)
  (goto-char (line-end-position))
  (newline)
  (indent-to col))

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

   ((s-matches? (rx symbol-start "match" symbol-end) (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (goto-char (max (line-beginning-position) (- (point) 2)))
    (yas-expand-snippet "| ${1:case} => $0"))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) (? "Co") "Inductive") (cb-buffers-current-line))
    (coq/newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert dependent type below the current one.
   ((s-matches? (rx bol (* space) "|" (* space) (+ word) (+ space) ":" (+ space))
                (cb-buffers-current-line))
    (coq/newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "=>") (cb-buffers-current-line))
    (coq/newline-and-indent-to-col (coq/case-start-col))
    (yas-expand-snippet "| ${1:case} => $0"))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (cb-buffers-current-line))
    (coq/newline-and-insert-at-col (coq/case-start-col) "| "))

   ;; Insert check.
   ((s-matches? (rx bol "Check") (cb-buffers-current-line))

    (coq/newline-and-indent-to-col (coq/rx-start-column "Check"))
    (yas-expand-snippet "Check $0."))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (evil-insert-state))

;;; funcs.el ends here
