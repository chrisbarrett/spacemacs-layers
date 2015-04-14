(defvar cb-haskell-packages
  '(
    haskell-mode
    shm
    hindent
    button-lock pos-tip popup ; liquid-haskell dependencies
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-haskell-excluded-packages '(ghc) ; ghc-mod is currently broken
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t)
  )

(defun cb-haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-to-list 'completion-ignored-extensions ".hi"))
    :config
    (progn
      (setq haskell-process-suggest-haskell-docs-imports t)
      (setq haskell-process-use-presentation-mode t)
      (setq haskell-interactive-mode-scroll-to-bottom t)
      (setq haskell-interactive-popup-errors t)
      (setq haskell-interactive-mode-eval-pretty t)
      (setq haskell-interactive-prompt "\nλ> ")
      (setq haskell-process-show-debug-tips)
      (setq haskell-stylish-on-save t)

      (with-eval-after-load 'yasnippet
        (setq yas-snippet-dirs
              (--reject (s-matches? "/haskell-mode" it) yas-snippet-dirs)))

      (with-eval-after-load 'flycheck
        (add-hook 'haskell-interactive-mode-hook (lambda () (flycheck-mode -1))))

      (custom-set-faces
       '(haskell-operator-face
         ((t :italic nil))))

      (defun cb-haskell/set-local-hooks ()
        (add-hook 'before-save-hook 'haskell/unicode-before-save nil t)
        (add-hook 'evil-insert-state-exit-hook 'haskell/unicode-before-save nil t))

      (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)

      (with-eval-after-load 'haskell
        (diminish 'interactive-haskell-mode " λ"))

      (put 'haskell-mode 'evil-shift-width 2)
      (add-hook 'haskell-mode-hook 'haskell/configure-flyspell)))

  (use-package haskell-indentation
    :diminish haskell-indentation-mode
    :commands turn-on-haskell-indentation
    :init
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    :config
    (progn

      (defun cb-haskell/show-indentation-guides ()
        (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
          (haskell-indentation-enable-show-indentations)))

      (defun cb-haskell/hide-indentation-guides ()
        (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
          (haskell-indentation-disable-show-indentations)))

      ;; Show indentation guides for haskell-indentation only in insert state.
      (add-hook 'evil-normal-state-entry-hook 'cb-haskell/hide-indentation-guides)
      (add-hook 'evil-insert-state-entry-hook 'cb-haskell/show-indentation-guides)
      (add-hook 'evil-insert-state-exit-hook  'cb-haskell/hide-indentation-guides))))

(defun cb-haskell/init-shm ()
  (use-package shm
    :commands structured-haskell-mode
    :init
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    :config
    (progn
      (require 'shm-reformat)
      (setq shm-auto-insert-skeletons nil)

      (add-hook 'haskell-mode-hook 'haskell/init-shm-smart-ops-compat)

      (core/remap-face 'shm-current-face 'core/bg-hl-ok)
      (core/remap-face 'shm-quarantine-face 'core/bg-hl-red))))

(defun cb-haskell/init-hindent ()
  (use-package hindent
    :config
    (setq hindent-style "gibiansky")))

(use-package ghc
  :commands (ghc-case-split)
  :defer t
  :config
  (defadvice ghc-check-syntax (around no-op activate)))

(defun cb-haskell/init-button-lock ()
  (use-package button-lock
    :diminish button-lock-mode
    :defer t))
