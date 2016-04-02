;;; packages.el --- cb-diff Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-diff-packages
  '(ediff
    (ediff-trees :location local)))

(defun cb-diff/post-init-ediff ()
  (defun cb-diff/turn-off-aggressive-indent ()
    (when (fboundp 'aggressive-indent-mode)
      (aggressive-indent-mode -1)))
  (add-hook 'diff-auto-refine-mode-hook #'cb-diff/turn-off-aggressive-indent))

(defun cb-diff/init-ediff-trees ()
  (use-package ediff-trees
    :commands (ediff-trees)))
