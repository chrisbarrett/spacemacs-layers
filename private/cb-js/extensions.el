;;; extensions.el --- cb-js Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-js-pre-extensions
  '(js))

(defconst cb-js-post-extensions
  '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-js/init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))
