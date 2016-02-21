;;; packages.el --- cb-haskell Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t)
  (require 'noflet nil t))

(defconst cb-haskell-packages
  '(
    haskell-mode
    shm
    hindent
    ghc
    smartparens
    flycheck
    smart-ops
    aggressive-indent
    llvm-mode
    cb-buffers

    (ghc-dump :location local)
    (haskell-ctrl-c-ctrl-c :location local)
    (haskell-flyspell :location local)
    (haskell-ghc-opts :location local)
    (haskell-imports :location local)
    (haskell-meta-ret :location local)
    (haskell-parser :location local)
    (haskell-pragmas :location local)
    (haskell-ret :location local)
    (haskell-unicode :location local)
    (haskell-snippets :excluded t)))

(defun cb-haskell/init-llvm-mode ()
  (use-package llvm-mode))

(defun cb-haskell/init-ghc-dump ()
  (use-package ghc-dump
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "D" #'ghc-dump-popup)
      (bind-key "q" #'cb-buffers-maybe-kill ghc-dump-popup-mode-map))))

(defun cb-haskell/post-init-flycheck ()
  (with-eval-after-load 'flycheck

    ;;; HACK: shouldn't have to do this.
    (defun cb-haskell/update-flycheck-ghc-language-extensions ()
      (when (derived-mode-p 'haskell-mode)
        (let ((exts (haskell-pragmas-in-file)))
          (setq-local flycheck-ghc-language-extensions exts)
          (setq-local flycheck-hlint-language-extensions exts))))

    (add-hook 'haskell-mode-hook #'cb-haskell/update-flycheck-ghc-language-extensions)
    (add-hook 'after-save-hook #'cb-haskell/update-flycheck-ghc-language-extensions)
    (add-hook 'haskell-interactive-mode-hook (lambda () (flycheck-mode -1)))))

(defun cb-haskell/post-init-haskell-mode ()
  (setq haskell-process-suggest-add-package t)
  (setq haskell-process-suggest-language-pragmas t)
  (setq haskell-process-suggest-hoogle-imports nil)
  (setq haskell-process-suggest-hayoo-imports nil)
  (setq haskell-process-suggest-haskell-docs-imports t)

  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-use-presentation-mode t)
  (setq haskell-interactive-mode-scroll-to-bottom t)
  (setq haskell-interactive-popup-errors t)
  (setq haskell-interactive-prompt "\nλ> ")
  (setq haskell-process-show-debug-tips nil)
  (setq haskell-stylish-on-save t)

  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-starter-offset 2)
  (setq haskell-indentation-where-pre-offset 2)
  (setq haskell-indentation-where-post-offset 2)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-ifte-offset 4)
  (setq haskell-indent-spaces 4)

  (setq haskell-import-mapping
        '(("Data.Map" . "import qualified Data.Map as Map\nimport Data.Map (Map)")
          ("Data.Maybe" . "import qualified Data.Maybe as Maybe")
          ("Data.Vector" . "import qualified Data.Vector as Vector\nimport Data.Vector (Vector)")
          ("Data.Text" . "import qualified Data.Text as T\nimport Data.Text (Text)")))

  (setq haskell-language-extensions
        '("-XUnicodeSyntax" "-XLambdaCase"))

  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".gm")

  (defun cb-haskell/maybe-haskell-interactive-mode ()
    (unless (bound-and-true-p org-src-mode)
      (interactive-haskell-mode)))

  (add-hook 'haskell-mode-hook #'cb-haskell/maybe-haskell-interactive-mode)

  (defun cb-haskell/set-local-hooks ()
    (setq evil-shift-width 4)
    (setq tab-width 4))

  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'cb-haskell/set-local-hooks)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)

  (custom-set-faces
   '(haskell-interactive-face-compile-error ((t (:foreground nil))))
   '(haskell-operator-face ((t :italic nil))))

  (defun cb-haskell/interactive-apply-font-locking ()
    (font-lock-add-keywords nil `(("Compilation failed." . '(face nil :foreground compilation-error-face)))))

  (add-hook 'haskell-interactive-mode-hook #'cb-haskell/interactive-apply-font-locking)

  (with-eval-after-load 'haskell
    (diminish 'interactive-haskell-mode " λ"))

  (with-eval-after-load 'haskell-mode
    (when (require 'ghc nil t)
      (define-key haskell-mode-map (kbd "C-c C-s") #'ghc-case-split))

    (evil-define-key 'insert haskell-mode-map (kbd "<backspace>") #'haskell/backspace)
    (evil-define-key 'normal haskell-mode-map (kbd "<backspace>") nil)
    (evil-define-key 'normal haskell-mode-map (kbd "<backtab>") #'haskell-indentation-indent-backwards)
    (evil-define-key 'normal haskell-mode-map (kbd "TAB") #'haskell-indentation-indent-line)
    (define-key haskell-mode-map (kbd "<backtab>") #'haskell-indentation-indent-backwards)
    (define-key haskell-mode-map (kbd "TAB") #'haskell-indentation-indent-line)

    (define-key haskell-mode-map (kbd "M-,")          #'pop-tag-mark)
    (define-key haskell-mode-map (kbd "M-P")          #'flymake-goto-prev-error)
    (define-key haskell-mode-map (kbd "M-N")          #'flymake-goto-next-error)
    (define-key haskell-mode-map (kbd "C-,")          #'haskell-move-nested-left)
    (define-key haskell-mode-map (kbd "C-.")          #'haskell-move-nested-right)
    (define-key haskell-mode-map (kbd "C-c C-d")      #'haskell-w3m-open-haddock)
    (define-key haskell-mode-map (kbd "C-c C-f")      #'haskell-cabal-visit-file)
    (define-key haskell-mode-map (kbd "C-c C-h")      #'haskell-hoogle)
    (define-key haskell-mode-map (kbd "C-c C-k")      #'haskell-interactive-mode-clear)
    (define-key haskell-mode-map (kbd "<backspace>")  #'haskell/backspace)
    (define-key haskell-mode-map (kbd "C-c i") 'shm-reformat-decl))

  (with-eval-after-load 'haskell-presentation-mode
    (evil-define-key 'normal haskell-presentation-mode-map (kbd "q") #'quit-window))

  (with-eval-after-load 'haskell-interactive-mode
    (define-key haskell-interactive-mode-map (kbd "C-c C-h") #'haskell-hoogle)
    (evil-define-key 'normal haskell-error-mode-map (kbd "q") #'quit-window)
    (evil-define-key 'normal haskell-mode-map (kbd "<return>") #'haskell-process-do-info)
    (evil-define-key 'insert haskell-interactive-mode-map (kbd "SPC") #'haskell/interactive-smart-space)
    (evil-define-key 'insert haskell-interactive-mode-map (kbd "<backspace>") #'haskell/backspace)
    (evil-define-key 'normal interactive-haskell-mode-map (kbd "M-.") #'haskell-mode-goto-loc)
    (evil-define-key 'normal interactive-haskell-mode-map (kbd ",t") #'haskell-mode-show-type-at))

  (with-eval-after-load 'haskell-cabal-mode
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear)))

(defun cb-haskell/post-init-shm ()
  (setq shm-auto-insert-skeletons nil)

  (core/remap-face 'shm-current-face 'core/bg-hl-ok)
  (core/remap-face 'shm-quarantine-face 'core/bg-hl-red)

  ;; Disable most shm bindings
  (defconst shm-repl-map (make-sparse-keymap))
  (defconst shm-map (make-sparse-keymap))

  (defun cb-haskell/configure-shm ()
    (require 'shm)
    (evil-define-key 'insert shm-map (kbd "<return>") 'haskell/ret)
    (define-key shm-map (kbd "C-<return>") 'shm/newline-indent)
    (define-key shm-map (kbd "SPC") 'haskell/smart-space))

  (add-hook 'haskell-mode-hook #'cb-haskell/configure-shm)

  (defun cb-haskell/structured-haskell-mode-off ()
    (when (and (boundp 'structured-haskell-mode) structured-haskell-mode)
      (ignore-errors
        (structured-haskell-mode -1))))

  (add-hook 'ghc-core-mode-hook #'cb-haskell/structured-haskell-mode-off)
  (add-hook 'ghc-stg-mode-hook #'cb-haskell/structured-haskell-mode-off))

(defun cb-haskell/post-init-ghc ()
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

      (with-eval-after-load 'haskell-mode
        (define-key haskell-mode-map (kbd "C-c C-r") 'ghc-refine)
        (define-key haskell-mode-map (kbd "C-c C-a") 'ghc-auto)

        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
        (define-key haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
        (define-key haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature)
        (define-key haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature)))))

(defun cb-haskell/post-init-smartparens ()
  (use-package smartparens
    :config
    (progn
      ;; FIX: Ensure Smartparens functions do not trigger indentation as a side-effect.

      (defun cb-haskell/hacky-sp-preserve-indent-level (original-fn &rest args)
        (if (derived-mode-p 'haskell-mode)
            (let ((col (current-indentation)))
              (atomic-change-group
                (apply original-fn args)
                (save-excursion
                  (goto-char (line-beginning-position))
                  (delete-horizontal-space)
                  (indent-to col))))
          (apply original-fn args)))

      (dolist (fn '(sp-kill-sexp
                    sp-unwrap-sexp
                    sp-forward-slurp-sexp
                    sp-backward-slurp-sexp
                    sp-forward-barf-sexp
                    sp-backward-barf-sexp
                    sp-join-sexp
                    sp-absorb-sexp
                    sp-splice-sexp
                    sp-splice-sexp-killing-around
                    sp-splice-sexp-killing-forward
                    sp-splice-sexp-killing-backward))
        (advice-add fn :around #'cb-haskell/hacky-sp-preserve-indent-level)))))

(defun cb-haskell/post-init-smart-ops ()
  (defun cb-haskell/reformat-comment-at-point ()
    (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
                 (_ (equal op "{"))
                 (_ (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                                (buffer-substring beg end))))
      (goto-char beg)
      (delete-region beg end)
      (insert "{- ") (save-excursion (insert " -}"))))

  (defun cb-haskell/reformat-pragma-at-point ()
    (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
                 (_ (equal op "{"))
                 (_ (s-matches? (rx bos "{" (* (any "-" space "#")) "}" eos)
                                (buffer-substring beg end))))
      (goto-char beg)
      (delete-region beg end)
      (insert "{-# ") (save-excursion (insert " #-}"))))

  (defun cb-haskell/indent-if-in-exports ()
    (when (ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
      (haskell-indentation-indent-line)))

  (defconst cb-haskell/smart-ops
    (-flatten-n 1
                (list
                 (smart-ops "->" "=>")
                 (smart-ops "$" "=" "~" "^" ":" "?")
                 (smart-ops "^~" :pad-before nil :pad-after nil)
                 (smart-op ";"
                           :pad-before nil :pad-after t)
                 (smart-ops ","
                            :pad-before nil :pad-after t
                            :action
                            #'cb-haskell/indent-if-in-exports)
                 (smart-op "-"
                           :action #'cb-haskell/reformat-comment-at-point)
                 (smart-op "#"
                           :pad-before nil :pad-after nil
                           :action #'cb-haskell/reformat-pragma-at-point)
                 (smart-ops-default-ops))))

  (define-smart-ops-for-mode 'haskell-mode
    cb-haskell/smart-ops)

  (define-smart-ops-for-mode 'haskell-interactive-mode
    (smart-op ":" :pad-unless (lambda (_) (haskell-interactive-at-prompt)))
    cb-haskell/smart-ops)

  ;; HACK: Enable smart ops for `haskell-mode' manually, since it is not derived
  ;; from `prog-mode'.
  (add-hook 'haskell-mode-hook #'smart-ops-mode))

(defun cb-haskell/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode)))

(defun cb-haskell/post-init-cb-buffers ()
  (use-package cb-buffers
    :config
    (add-to-list 'cb-buffers-indent-commands-alist '(haskell-mode . haskell/format-dwim))))

(defun cb-haskell/init-haskell-ctrl-c-ctrl-c ()
  (use-package haskell-ctrl-c-ctrl-c
    :commands haskell-ctrl-c-ctrl-c-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-ctrl-c-ctrl-c-init)))

(defun cb-haskell/init-haskell-flyspell ()
  (use-package haskell-flyspell
    :commands haskell-flyspell-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-flyspell-init)))

(defun cb-haskell/init-haskell-ghc-opts ()
  (use-package haskell-ghc-opts
    :commands haskell-ghc-opts-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-ghc-opts-init)))

(defun cb-haskell/init-haskell-imports ()
  (use-package haskell-imports
    :commands haskell-imports-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-imports-init)))

(defun cb-haskell/init-haskell-meta-ret ()
  (use-package haskell-meta-ret
    :commands haskell-meta-ret-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-meta-ret-init)))

(defun cb-haskell/init-haskell-parser ()
  (with-eval-after-load 'haskell-mode
    (require 'haskell-parser)))

(defun cb-haskell/init-haskell-pragmas ()
  (use-package haskell-pragmas
    :commands haskell-pragmas-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-pragmas-init)))

(defun cb-haskell/init-haskell-ret ()
  (use-package haskell-ret
    :commands haskell-ret-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-ret-init)))

(defun cb-haskell/init-haskell-unicode ()
  (use-package haskell-unicode
    :commands haskell-unicode-init
    :init
    (add-hook 'haskell-mode-hook #'haskell-unicode-init)))

;;; packages.el ends here
