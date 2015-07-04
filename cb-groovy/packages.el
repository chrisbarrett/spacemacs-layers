;;; packages.el --- cb-groovy layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-groovy-packages '(groovy-mode))

(defconst cb-groovy-excluded-packages '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :mode ("\\.groovy$" . groovy-mode)
    :config
    (progn
      (setq groovy-home "/usr/local/"))))
