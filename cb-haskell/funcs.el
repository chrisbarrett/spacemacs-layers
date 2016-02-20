;;; funcs.el --- Functions for cb-haskell layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'thingatpt)

(defun haskell/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "{-#" "(#" "{-@") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun haskell/before-subexp-closing? ()
  (s-matches? (rx bol (? ">") (* space) (or "}" "]" "-}" "#-}" "@-}" "#)"))
              (buffer-substring (point) (line-end-position))))

(defun haskell/smart-space ()
  "Use shm space, but perform extra padding inside lists."
  (interactive)
  (cond
   ((and (haskell/after-subexpr-opening?) (haskell/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (sp/generic-prog-space))))

(defun haskell/interactive-smart-space ()
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (haskell-interactive-mode-space))
   ((and (haskell/after-subexpr-opening?) (haskell/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun haskell/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (cond
   ((and (haskell/after-subexpr-opening?)
         (haskell/before-subexp-closing?)
         (thing-at-point-looking-at (rx (+ space))))
    (delete-horizontal-space))

   ((and (s-matches? (rx (or "{-#" "{-@" "{-" "(#") eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (? ">") (or "@-}" "#-}" "-}" "#)"))
                     (buffer-substring (point) (line-end-position))))
    (delete-char 1)
    (delete-char -1))

   (t
    (sp/generic-prog-backspace))))

(defun haskell/format-dwim ()
  (interactive "*")
  (hindent/reformat-decl)
  (haskell-mode-stylish-buffer)
  (haskell/unicode-buffer))

;;; funcs.el ends here
