;;; packages.el --- cb-haskell Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-haskell-packages
  '(
    haskell-mode
    shm
    hindent
    button-lock pos-tip popup ; liquid-haskell dependencies
    ghc
    smartparens
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-haskell-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

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
        (add-hook 'before-save-hook 'haskell/rewrite-symbols-in-buffer nil t)
        (add-hook 'evil-insert-state-exit-hook 'haskell/rewrite-symbols-in-buffer nil t))

      (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

      (with-eval-after-load 'haskell
        (diminish 'interactive-haskell-mode " λ"))

      (put 'haskell-mode 'evil-shift-width 2)
      (add-hook 'haskell-mode-hook 'haskell/configure-flyspell)


      (evil-define-key 'normal haskell-mode-map (kbd "SPC i i") 'haskell/insert-import)
      (evil-define-key 'normal haskell-mode-map (kbd "SPC i q") 'haskell/insert-qualified-import)
      (evil-define-key 'normal haskell-mode-map (kbd "SPC i l") 'haskell/insert-language-pragma)
      (evil-define-key 'normal haskell-mode-map (kbd "SPC i o") 'haskell/insert-ghc-option)

      (evil-define-key 'normal haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)
      (evil-define-key 'insert haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)
      (define-key haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)

      (evil-define-key 'normal haskell-mode-map (kbd "<backtab>") 'haskell-indentation-indent-backwards)
      (evil-define-key 'normal haskell-mode-map (kbd "TAB") 'haskell-indentation-indent-line)
      (define-key haskell-mode-map (kbd "<backtab>") 'haskell-indentation-indent-backwards)
      (define-key haskell-mode-map (kbd "TAB") 'haskell-indentation-indent-line)

      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-c") 'cb-haskell/C-c-C-c)
      (define-key haskell-mode-map (kbd "C-c C-c") 'cb-haskell/C-c-C-c)

      (define-key haskell-mode-map (kbd "M-,")           'pop-tag-mark)
      (define-key haskell-mode-map (kbd "M-P")           'flymake-goto-prev-error)
      (define-key haskell-mode-map (kbd "M-N")           'flymake-goto-next-error)
      (define-key haskell-mode-map (kbd "C-,")           'haskell-move-nested-left)
      (define-key haskell-mode-map (kbd "C-.")           'haskell-move-nested-right)
      (define-key haskell-mode-map (kbd "C-c C-d")       'haskell-w3m-open-haddock)
      (define-key haskell-mode-map (kbd "C-c C-f")       'haskell-cabal-visit-file)
      (define-key haskell-mode-map (kbd "C-c C-h")       'haskell-hoogle)
      (define-key haskell-mode-map (kbd "C-c C-c")       'haskell-process-cabal-build)
      (define-key haskell-mode-map (kbd "C-c C-k")       'haskell-interactive-mode-clear)
      (define-key haskell-mode-map (kbd "<backspace>")   'haskell/backspace)
      (define-key haskell-mode-map (kbd "C-c i") 'shm-reformat-decl)

      (define-key haskell-mode-map (kbd "#") 'haskell/smart-hash)


      (with-eval-after-load 'haskell-presentation-mode
        (evil-define-key 'normal haskell-presentation-mode-map (kbd "q") 'quit-window))

      (with-eval-after-load 'haskell-interactive-mode
        (define-key haskell-interactive-mode-map (kbd "C-c C-h") 'haskell-hoogle)
        (evil-define-key 'normal haskell-error-mode-map (kbd "q") 'quit-window)

        (evil-define-key 'normal haskell-mode-map (kbd "<return>") 'haskell-process-do-info)

        (evil-define-key 'insert haskell-interactive-mode-map (kbd "SPC") 'haskell/interactive-smart-space)

        (evil-define-key 'insert haskell-interactive-mode-map (kbd "<backspace>") 'haskell/backspace)

        (evil-define-key 'normal interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
        (evil-define-key 'normal interactive-haskell-mode-map (kbd ",t") 'haskell-mode-show-type-at))

      (with-eval-after-load 'haskell-cabal-mode
        (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear))

      ))

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
    (progn
      (add-hook 'haskell-mode-hook 'structured-haskell-mode)
      (add-hook 'ghc-core-mode-hook (lambda () (structured-haskell-mode -1))))
    :config
    (progn
      (require 'shm-reformat)
      (setq shm-auto-insert-skeletons nil)

      (add-hook 'haskell-mode-hook 'haskell/init-shm-smart-ops-compat)

      (core/remap-face 'shm-current-face 'core/bg-hl-ok)
      (core/remap-face 'shm-quarantine-face 'core/bg-hl-red)

      (evil-define-key 'normal shm-map "J" 'haskell/join-line)
      (evil-define-key 'insert shm-map (kbd "<return>") 'haskell/ret)
      (evil-define-key 'normal shm-map (kbd "M-RET") nil)

      (define-key shm-map (kbd "C-<return>") 'shm/newline-indent)
      (define-key shm-map (kbd "<return>") nil)
      (define-key shm-map (kbd "SPC") 'haskell/smart-space)

      ;; undefine commands that interfere with smart ops, etc.
      (define-key shm-map (kbd "C-c C-s") nil)
      (define-key shm-map (kbd ")") nil)
      (define-key shm-map (kbd ",") nil)
      (define-key shm-map (kbd ":") nil)
      (define-key shm-map (kbd "#") nil)
      (define-key shm-map (kbd "-") nil)
      (define-key shm-map (kbd "DEL") nil)
      (define-key shm-map (kbd "C-<backspace>") nil)
      (define-key shm-map (kbd "<backtab>") nil)
      (define-key shm-map (kbd "TAB") nil))))

(defun cb-haskell/init-hindent ()
  (use-package hindent
    :config
    (setq hindent-style "gibiansky")))

(defun cb-haskell/init-ghc ()
  (use-package ghc
    :commands (ghc-case-split)
    :defer t
    :config
    (progn
      ;; HACK: Redefine asshole init function so it doesn't clobber the major
      ;; mode's map.
      (defun ghc-init ()
        (ghc-abbrev-init)
        (ghc-type-init)
        (unless ghc-initialized
          (ghc-comp-init)
          (setq ghc-initialized t))
        (ghc-import-module))

      (defadvice ghc-check-syntax (around no-op activate))

      (require 'haskell-mode)

      (define-key haskell-mode-map (kbd "C-c C-s") 'ghc-case-split)
      (define-key haskell-mode-map (kbd "C-c C-r") 'ghc-refine)
      (define-key haskell-mode-map (kbd "C-c C-a") 'ghc-auto)

      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
      (define-key haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
      (define-key haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature)
      (define-key haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature))))

(defun cb-haskell/init-button-lock ()
  (use-package button-lock
    :diminish button-lock-mode
    :defer t))

(defun cb-haskell/init-smartparens ()
  (use-package smartparens
    :config
    (progn
      ;; FIX: Ensure Smartparens functions do not trigger indentation as a side-effect.

      (defmacro cb-haskell/sp-advise-to-preserve-indent-level (fname)
        `(defadvice ,fname (around preserve-indentation activate)
           (if (derived-mode-p 'haskell-mode)
               (let ((col (current-indentation)))
                 (atomic-change-group
                   ad-do-it
                   (save-excursion
                     (goto-char (line-beginning-position))
                     (delete-horizontal-space)
                     (indent-to col))))
             ad-do-it)))

      (cb-haskell/sp-advise-to-preserve-indent-level sp-kill-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-unwrap-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-forward-slurp-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-backward-slurp-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-forward-barf-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-backward-barf-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-join-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-absorb-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-splice-sexp)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-splice-sexp-killing-around)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-splice-sexp-killing-forward)
      (cb-haskell/sp-advise-to-preserve-indent-level sp-splice-sexp-killing-backward))))
