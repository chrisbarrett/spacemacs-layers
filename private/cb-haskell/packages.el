(defvar cb-haskell-packages
  '(
    ;; package cb-haskells go here
    haskell-mode
    shm
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
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-to-list 'completion-ignored-extensions ".hi"))
    :config
    (progn
      (setq haskell-process-suggest-haskell-docs-imports t)
      (setq haskell-process-suggest-remove-import-lines t)
      (setq haskell-process-use-presentation-mode t)
      (setq haskell-interactive-mode-scroll-to-bottom t)
      (setq haskell-interactive-mode-eval-pretty t)
      (setq haskell-interactive-prompt "\nλ> ")
      (setq haskell-process-path-ghci "ghci-ng")

      (defun cb-haskell/set-local-hooks ()
        (add-hook 'before-save-hook 'haskell/unicode-before-save nil t))

      (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)

      (after 'haskell
        (diminish 'interactive-haskell-mode "H.λ"))

      (when (executable-find "ghci-ng")
        (add-to-list 'haskell-process-args-cabal-repl "--with-ghc=ghci-ng"))

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

(defun cb-haskell/init-company-ghc ()
  (use-package company-ghc
    :defer t
    :config
    (progn
      (setq company-ghc-show-info nil)
      (setq company-ghc-show-module t))))
