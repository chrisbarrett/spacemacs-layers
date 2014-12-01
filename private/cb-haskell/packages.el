(defvar cb-haskell-packages
  '(
    ;; package cb-haskells go here
    haskell-mode
    flycheck-haskell
    company-ghc
    shm
    hi2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-haskell-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-haskell/init-<package-cb-haskell>
;;
;; (defun cb-haskell/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (add-to-list 'completion-ignored-extensions ".hi"))
    :config
    (progn
      (require 'company-ghc)

      (setq haskell-process-suggest-hoogle-imports t)
      (setq haskell-tags-on-save t)
      (setq haskell-process-suggest-remove-import-lines t)
      (setq haskell-process-auto-import-loaded-modules t)
      (setq haskell-process-suggest-hoogle-imports t)
      (setq haskell-stylish-on-save t)
      (setq haskell-program-name "ghci")
      (setq haskell-process-type 'cabal-repl)
      (setq haskell-interactive-prompt "\nλ> ")

      (put 'haskell-mode 'tab-width 2)
      (put 'haskell-mode 'evil-shift-width 2)
      (add-hook 'haskell-mode-hook 'haskell/configure-flyspell))))

(defun cb-haskell/init-shm ()
  (use-package shm
    :commands structured-haskell-mode
    :init
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    :config
    (progn
      (require 'shm-case-split)
      (setq shm-auto-insert-skeletons nil)

      (add-hook 'haskell-mode-hook 'haskell/init-shm-smart-ops-compat)

      (custom-set-faces
       '(shm-current-face
         ((((background dark))  :background "#01304b")
          (((background light)) :background "#e9f2c5")))
       '(shm-quarantine-face
         ((((background dark))  :background "#51202b")
          (((background light)) :background "#fee8e5")))))))

(defun cb-haskell/init-company-ghc ()
  (use-package company-ghc
    :defer t
    :config
    (progn
      (require 'company)
      (add-to-list 'company-backends 'company-ghc)
      (setq company-ghc-show-info nil)
      (setq company-ghc-show-module t))))

(defun cb-haskell/init-hi2 ()
  (use-package hi2
    :diminish hi2-mode
    :commands turn-on-hi2
    :init
    (add-hook 'haskell-mode-hook 'turn-on-hi2)
    :config
    (progn
      ;; Show indentation guides for hi2 only in insert state.
      (add-hook 'evil-normal-state-entry-hook 'haskell/hide-hi2-guides)
      (add-hook 'evil-insert-state-entry-hook 'haskell/show-hi2-guides)
      (add-hook 'evil-insert-state-exit-hook 'haskell/hide-hi2-guides))))
