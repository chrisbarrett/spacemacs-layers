;;; funcs.el --- Supporting functions for js config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 's nil t)
  (require 'dash nil t))

(defun js/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun js/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]"))
              (buffer-substring (point) (line-end-position))))

(defun js/space ()
  "Insert a space, performing extra padding inside lists."
  (interactive)
  (cond
   ((and (js/after-subexpr-opening?) (js/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun js/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((and (js/after-subexpr-opening?)
          (js/before-subexp-closing?)
          (thing-at-point-looking-at (rx (+ space))))
     (delete-horizontal-space))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))

(defun js/ret (&optional arg)
  "Insert a newline with context-sensitive formatting."
  (interactive "P")
  (cond
   ((or arg (core/in-string-or-comment?))
    (comment-indent-new-line)
    (just-one-space))

   ((sp/between-curly-braces? t)
    (sp/split-braced-expression-over-new-lines (rx (or ";" ","))))

   (t
    (call-interactively 'comment-indent-new-line))))
