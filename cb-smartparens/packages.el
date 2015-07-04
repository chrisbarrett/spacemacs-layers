;;; packages.el --- cb-smartparens Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-smartparens-packages
  '(smartparens)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-smartparens-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-smartparens/init-smartparens ()
  (use-package smartparens
    :config
    (progn
      (setq sp-navigate-close-if-unbalanced t)
      (setq sp-message-width nil)

      (add-hook 'minibuffer-setup-hook 'sp/maybe-enable-smartparens t)
      (add-hook 'minibuffer-inactive-mode-hook 'sp/maybe-enable-smartparens t)

      (add-hook 'smartparens-mode-hook 'sp/hacky-set-sp-bindings t)
      (add-hook 'smartparens-strict-mode-hook 'sp/hacky-set-sp-bindings t)

      (smartparens-global-strict-mode +1)
      (show-smartparens-global-mode +1))))
