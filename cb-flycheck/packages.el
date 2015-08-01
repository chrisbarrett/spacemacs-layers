;;; packages.el --- cb-flycheck Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-flycheck-packages
  '(flycheck)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-flycheck-excluded-packages '(flycheck-pos-tip)
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-flycheck/init-flycheck ()
  (use-package flycheck
    :config
    (progn
      (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
      (setq flycheck-display-errors-delay 0)
      (setq flycheck-check-syntax-automatically '(mode-enabled idle-change save))
      (global-flycheck-mode +1))))
