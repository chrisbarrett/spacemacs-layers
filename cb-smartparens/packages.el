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

      ;; HACK: remove hook set by Spacemacs. This hook could leave smartparens
      ;; disabled if a snippet was exited under unusual circumstances.
      (defvar smartparens-enabled-initially t)
      (remove-hook 'yas-before-expand-snippet-hook (lambda ()
                                                     ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                                                     (setq smartparens-enabled-initially smartparens-mode)
                                                     (smartparens-mode -1)))

      (setq sp-navigate-close-if-unbalanced t)
      (setq sp-message-width nil)

      (add-hook 'minibuffer-setup-hook 'sp/maybe-enable-smartparens t)
      (add-hook 'minibuffer-inactive-mode-hook 'sp/maybe-enable-smartparens t)

      (add-hook 'smartparens-mode-hook 'sp/hacky-set-sp-bindings t)
      (add-hook 'smartparens-strict-mode-hook 'sp/hacky-set-sp-bindings t)

      (smartparens-global-strict-mode +1)
      (show-smartparens-global-mode +1))))
