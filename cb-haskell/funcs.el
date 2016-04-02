;;; funcs.el --- Functions for cb-haskell layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun haskell/format-dwim ()
  (interactive "*")
  (hindent/reformat-decl)
  (haskell-mode-stylish-buffer)
  (haskell/unicode-buffer))

;;; funcs.el ends here
