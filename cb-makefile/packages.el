;;; packages.el --- Makefile packages.el for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst cb-makefile-packages '(make-mode))

(defun cb-makefile/init-make-mode ()
  (use-package make-mode
    :bind
    (:map makefile-mode-map ("C-c C-c" . compile))))

;;; packages.el ends here
