(defvar cb-haskell-packages
  '(
    haskell-mode
    shm
    hindent
    hi2
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

      (custom-set-faces
       '(haskell-operator-face
         ((t :italic nil))))

      (defun cb-haskell/set-local-hooks ()
        (add-hook 'before-save-hook 'haskell/unicode-before-save nil t)
        (add-hook 'evil-insert-state-exit-hook 'haskell/unicode-before-save nil t))

      (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)

      (after 'haskell
        (diminish 'interactive-haskell-mode " λ"))

      (put 'haskell-mode 'evil-shift-width 2)
      (add-hook 'haskell-mode-hook 'haskell/configure-flyspell))))

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

      (custom-set-faces
       '(shm-current-face
         ((((background dark))  :background "#01304b")
          (((background light)) :background "#e9f2c5")))
       '(shm-quarantine-face
         ((((background dark))  :background "#51202b")
          (((background light)) :background "#fee8e5")))))))

(defun cb-haskell/init-hindent ()
  (use-package hindent
    :config
    (setq hindent-style "gibiansky")))

(defun cb-haskell/init-hi2 ()
  (use-package hi2
    :diminish hi2-mode
    :commands turn-on-hi2
    :init
    (add-hook 'haskell-mode-hook 'turn-on-hi2)
    :config
    (progn

      (defun spacemacs/haskell-show-hi2-guides ()
        (when (and (boundp 'hi2-mode) hi2-mode)
          (hi2-enable-show-indentations)))

      (defun spacemacs/haskell-hide-hi2-guides ()
        (when (and (boundp 'hi2-mode) hi2-mode)
          (hi2-disable-show-indentations)))

      ;; Show indentation guides for hi2 only in insert state.
      (add-hook 'evil-normal-state-entry-hook 'spacemacs/haskell-hide-hi2-guides)
      (add-hook 'evil-insert-state-entry-hook 'spacemacs/haskell-show-hi2-guides)
      (add-hook 'evil-insert-state-exit-hook  'spacemacs/haskell-hide-hi2-guides))))

(defun cb-haskell/init-ghc ()
  (use-package ghc
    :commands (ghc-case-split)
    :disabled t
    :defer t
    :config
    (defadvice ghc-init (around no-keybindings activate)
      (noflet ((define-key (&rest _)))
        ad-do-it))))

(defun cb-haskell/init-button-lock ()
  (use-package button-lock
    :diminish button-lock-mode
    :defer t))
