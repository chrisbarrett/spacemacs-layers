;;; packages.el --- Makefile packages.el for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst cb-makefile-packages '(make-mode))

(defun cb-makefile/init-make-mode ()
  (with-eval-after-load 'make-mode
    (define-key makefile-mode-map (kbd "C-c C-c") 'compile)))

;;; packages.el ends here
