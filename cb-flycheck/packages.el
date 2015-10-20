;;; packages.el --- cb-flycheck Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-flycheck-packages
  '(flycheck
    (flycheck-pos-tip :excluded t)))

(defun cb-flycheck/post-init-flycheck ()
  (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-display-errors-delay 0.5)
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-change save))
  (global-flycheck-mode +1))
