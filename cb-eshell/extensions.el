;;; extensions.el --- cb-eshell Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-eshell-pre-extensions '(eshell))

(defconst cb-eshell-post-extensions '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-eshell/init-eshell ()
  (use-package eshell
    :defer t
    :init
    (global-set-key (kbd "<f1>") 'cb-eshell-bring)))
