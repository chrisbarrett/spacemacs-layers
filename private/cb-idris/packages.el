;;; packages.el --- cb-idris Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-idris-packages
  '(idris-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-idris-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-idris/init-idris-mode ()
  (use-package idris-mode
    :mode "\\.idr\\'"
    :init
    (progn
      (add-to-list 'completion-ignored-extensions ".ibc")

      (defvar idris-mode-hook
        '(turn-on-idris-simple-indent
          idris-enable-clickable-imports
          turn-on-eldoc-mode
          idris-define-loading-keys
          idris-define-docs-keys
          idris-define-editing-keys
          idris-define-general-keys
          idris-define-ipkg-keys
          idris-define-ipkg-opening-keys
          idris-define-evil-keys))

      )
    :config
    (progn
      (setq idris-warnings-printing 'warnings-repl)
      (setq idris-repl-animate nil)
      (setq idris-repl-prompt-style 'long)

      (put 'idris-mode 'evil-shift-width 2)

      (core/remap-face 'idris-semantic-type-face 'font-lock-type-face)
      (core/remap-face 'idris-semantic-data-face 'default)
      (core/remap-face 'idris-semantic-function-face 'font-lock-function-name-face)
      (core/remap-face 'idris-semantic-bound-face 'font-lock-variable-name-face)
      (core/remap-face 'idris-semantic-implicit-face 'font-lock-comment-face)
      (core/remap-face 'idris-repl-output-face 'compilation-info)

      (add-to-list 'font-lock-keywords-alist
                   '(idris-mode
                     ((("^ *record\\>" . font-lock-keyword-face)))))


      ;; Advices

      (defadvice idris-mode (before start-process activate)
        "Automatically run an idris process."
        (unless idris-process
          (idris-run))))))
