;;; packages.el --- cb-diff Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-diff-packages
  '(ediff
    (ediff-trees :location local)))

(defun cb-diff/post-init-ediff ()
  (add-hook 'diff-auto-refine-mode-hook 'cb-core/turn-off-aggressive-indent-mode))

(defun cb-diff/init-ediff-trees ()
  (use-package ediff-trees
    :commands (ediff-trees)))
