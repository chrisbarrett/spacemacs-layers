(defvar cb-haskell-packages
  '(
    haskell-mode
    shm
    hindent
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-haskell-excluded-packages '()
  "List of packages to exclude.")

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
      (setq haskell-interactive-mode-eval-pretty t)
      (setq haskell-interactive-prompt "\nλ> ")

      (custom-set-faces
       '(haskell-operator-face
         ((t :italic nil))))

      (defun cb-haskell/set-local-hooks ()
        (add-hook 'before-save-hook 'haskell/unicode-before-save nil t))

      (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)

      (after 'haskell
        (diminish 'interactive-haskell-mode "λ"))

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
