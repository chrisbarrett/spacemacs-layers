;;; packages.el --- cb-diff Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-diff-packages
  '((ediff-trees :location local)))

(defun cb-diff/init-ediff-trees ()
  (use-package ediff-trees
    :commands (ediff-trees)))
