;;; extensions.el --- cb-haskell Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-haskell-pre-extensions
  '(
    haskell-parser
    )
  "List of all extensions to load before the packages.")

(defconst cb-haskell-post-extensions
  '(
    super-smart-ops
    liquid-types
    )
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t)
  (require 'super-smart-ops nil t))

(defun cb-haskell/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'haskell-mode
        :add '("$" "=" "~" "^")
        :custom
        '(("." . haskell/smart-dot)
          ("," . haskell/smart-comma)
          ("|" . haskell/smart-pipe)
          ("#" . haskell/smart-hash)
          ("@" . haskell/smart-at)
          ("-" . haskell/smart-minus)
          (":" . haskell/smart-colon)
          (";" . core/semicolon-then-space)))

      (super-smart-ops-configure-for-mode 'haskell-interactive-mode
        :add '("$" "=" "~" "^")
        :custom
        '(("." . haskell/smart-dot)
          ("-" . haskell/smart-minus)
          ("#" . haskell/smart-hash)
          ("@" . haskell/smart-at)
          ("|" . haskell/smart-pipe)
          (":" . haskell/ghci-smart-colon)
          ("," . haskell/ghci-smart-comma)
          (";" . core/semicolon-then-space))))))

(defun cb-haskell/init-haskell-parser ()
  (use-package haskell-parser
    :defer t
    :init
    (eval-after-load 'haskell-mode
      '(require 'haskell-parser))))

(defun cb-haskell/init-liquid-types ()
  (use-package liquid-types
    :defer t
    :init
    (progn
      (defun cb-haskell/maybe-init-liquid-haskell ()
        (when (executable-find "liquid")
          (require 'flycheck-liquid)
          (require 'liquid-tip)
          (flycheck-add-next-checker 'haskell-ghc 'haskell-hlint)
          ;; (flycheck-add-next-checker 'haskell-hlint 'haskell-liquid)
          ;;(flycheck-select-checker 'haskell-liquid)
          (liquid-tip-init 'ascii)))

      (add-hook 'haskell-mode-hook 'cb-haskell/maybe-init-liquid-haskell)
      (add-hook 'literate-haskell-mode-hook 'cb-haskell/maybe-init-liquid-haskell))))
