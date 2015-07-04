;;; extensions.el --- cb-flycheck Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-flycheck-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defconst cb-flycheck-post-extensions
  '(haskell-flycheck)
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-flycheck/init-haskell-flycheck ()
  (use-package haskell-flycheck
    :config
    (progn
      ;;; Use the Haskell session for syntax checking when available

      (defun cb-flycheck/configure-haskell-checker ()
        (flycheck-select-checker 'haskell-process)
        (flycheck-mode +1))

      (add-hook 'interactive-haskell-mode-hook 'cb-flycheck/configure-haskell-checker)
      )))
