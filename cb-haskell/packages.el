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
    button-lock pos-tip popup ; liquid-haskell dependencies
    ghc
    smartparens
    flycheck
    smart-ops
    aggressive-indent
    llvm-mode
    cb-buffers

    (ghc-dump :location local)
    (haskell-parser :location local)
    (haskell-snippets :excluded t)))

(defun cb-haskell/init-llvm-mode ()
  (use-package llvm-mode))

(defun cb-haskell/init-ghc-dump ()
  (use-package ghc-dump
    :config
    (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "mD" 'ghc-dump-popup)))

(defun cb-haskell/post-init-flycheck ()
  (with-eval-after-load 'flycheck

    ;;; HACK: shouldn't have to do this.
    (defun cb-haskell/update-flycheck-ghc-language-extensions ()
      (when (derived-mode-p 'haskell-mode)
        (let ((exts (haskell/language-pragmas-in-file)))
          (setq-local flycheck-ghc-language-extensions exts)
          (setq-local flycheck-hlint-language-extensions exts))))

    (add-hook 'haskell-mode-hook 'cb-haskell/update-flycheck-ghc-language-extensions)
    (add-hook 'after-save-hook 'cb-haskell/update-flycheck-ghc-language-extensions)
    (add-hook 'haskell-interactive-mode-hook (lambda () (flycheck-mode -1)))))

(defun cb-haskell/post-init-haskell-mode ()
  (setq haskell-process-suggest-add-package t)
  (setq haskell-process-suggest-language-pragmas t)
  (setq haskell-process-suggest-hoogle-imports nil)
  (setq haskell-process-suggest-hayoo-imports nil)
  (setq haskell-process-suggest-haskell-docs-imports t)

  ;; HACK: Advise suggestion functions to always just go ahead.

  (defun cb-haskell--silence-y-or-n-p (fn &rest args)
    "Stub `y-or-n-p' such that it always returns t."
    (noflet ((y-or-n-p (_) t))
      (apply fn args)))

  (advice-add 'haskell-process-suggest-add-package :around #'cb-haskell--silence-y-or-n-p)
  (advice-add 'haskell-process-suggest-imports :around #'cb-haskell--silence-y-or-n-p)
  (advice-add 'haskell-process-suggest-pragma :around #'cb-haskell--silence-y-or-n-p)

  (defun cb-haskell--cabal-add-dependency-never-prompts (fn &rest args)
    "Change the `no-prompt' positional argument to always be true."
    (-let [(package version _no-prompt sort silent) args]
      (prog1 (funcall fn package version t sort silent)
        (message "Added %s to cabal dependencies." package))))

  (advice-add 'haskell-cabal-add-dependency :around #'cb-haskell--cabal-add-dependency-never-prompts)


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

  (setq haskell-process-path-ghci
        (let ((ghci-ng (f-join user-home-directory ".local/bin/ghci-ng")))
          (if (f-executable? ghci-ng)
              ghci-ng
            (executable-find "ghci"))))

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

  (add-hook 'haskell-mode-hook 'cb-haskell/maybe-haskell-interactive-mode)

  (custom-set-faces
   '(haskell-operator-face
     ((t :italic nil))))

  (defun cb-haskell/set-local-hooks ()
    (setq evil-shift-width 4)
    (setq tab-width 4)
    (add-hook 'before-save-hook 'haskell/unicode-buffer nil t)
    (add-hook 'evil-insert-state-exit-hook 'haskell/unicode-buffer nil t))

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'cb-haskell/set-local-hooks)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  (with-eval-after-load 'haskell
    (diminish 'interactive-haskell-mode " λ"))

  (add-hook 'haskell-mode-hook 'haskell/configure-flyspell)

  (with-eval-after-load 'haskell-mode
    (evil-define-key 'insert haskell-mode-map (kbd "<backspace>") 'haskell/backspace)
    (evil-define-key 'normal haskell-mode-map (kbd "<backspace>") nil)

    (evil-define-key 'normal haskell-mode-map (kbd "SPC i i") 'haskell/insert-import)
    (evil-define-key 'normal haskell-mode-map (kbd "SPC i q") 'haskell/insert-qualified-import)
    (evil-define-key 'normal haskell-mode-map (kbd "SPC i L") 'haskell/insert-language-pragma)
    (evil-define-key 'normal haskell-mode-map (kbd "SPC i o") 'haskell/insert-ghc-option)

    (evil-define-key 'normal haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)
    (evil-define-key 'insert haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)
    (define-key haskell-mode-map (kbd "M-RET") 'haskell/meta-ret)

    (evil-define-key 'insert haskell-mode-map (kbd "<return>") 'haskell/ret)

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
    (define-key haskell-mode-map (kbd "C-c i") 'shm-reformat-decl))

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
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)))

(defun cb-haskell/post-init-shm ()
  (setq shm-auto-insert-skeletons nil)

  (core/remap-face 'shm-current-face 'core/bg-hl-ok)
  (core/remap-face 'shm-quarantine-face 'core/bg-hl-red)

  ;; Disable most shm bindings
  (defconst shm-repl-map (make-sparse-keymap))
  (defconst shm-map (make-sparse-keymap))

  (defun cb-haskell/configure-shm ()
    (require 'shm)
    (evil-define-key 'normal shm-map "J" 'haskell/join-line)
    (evil-define-key 'insert shm-map (kbd "<return>") 'haskell/ret)
    (define-key shm-map (kbd "C-<return>") 'shm/newline-indent)
    (define-key shm-map (kbd "SPC") 'haskell/smart-space))

  (add-hook 'haskell-mode-hook 'cb-haskell/configure-shm)
  (add-hook 'ghc-core-mode-hook (lambda () (structured-haskell-mode -1))))

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
        (define-key haskell-mode-map (kbd "C-c C-s") 'ghc-case-split)
        (define-key haskell-mode-map (kbd "C-c C-r") 'ghc-refine)
        (define-key haskell-mode-map (kbd "C-c C-a") 'ghc-auto)

        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
        (define-key haskell-mode-map (kbd "C-c C-n") 'ghc-goto-next-hole)
        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
        (define-key haskell-mode-map (kbd "C-c C-p") 'ghc-goto-prev-hole)
        (evil-define-key 'normal haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature)
        (define-key haskell-mode-map (kbd "C-c C-k") 'ghc-insert-template-or-signature)))))

(defun cb-haskell/init-button-lock ()
  (use-package button-lock
    :diminish button-lock-mode
    :defer t))

(defun cb-haskell/post-init-smartparens ()
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

  (defun cb-haskell/reformat-refinement-type-at-point ()
    (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
                 (_ (equal op "{"))
                 (_ (s-matches? (rx bos "{" (* (any "-" "@" space)) "}" eos)
                                (buffer-substring beg end))))
      (goto-char beg)
      (delete-region beg end)
      (insert "{-@ ") (save-excursion (insert " @-}"))))

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
                 (smart-op "@"
                           :pad-before nil
                           :pad-after nil
                           :action 'cb-haskell/reformat-refinement-type-at-point)
                 (smart-ops-default-ops))))

  (define-smart-ops-for-mode 'haskell-mode
    cb-haskell/smart-ops)

  (define-smart-ops-for-mode 'haskell-interactive-mode
    (smart-op ":" :pad-unless (lambda (_) (haskell-interactive-at-prompt)))
    cb-haskell/smart-ops)

  ;; HACK: Enable smart ops for `haskell-mode' manually, since it is not derived
  ;; from `prog-mode'.
  (add-hook 'haskell-mode-hook 'smart-ops-mode))

(defun cb-haskell/init-haskell-parser ()
  (with-eval-after-load 'haskell-mode
    (require 'haskell-parser)))

(defun cb-haskell/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode)))

(defun cb-haskell/init-liquid-types ()
  (use-package liquid-types
    :defer t
    :init
    (progn
      (defvar cb-haskell/use-liquid-haskell? nil)

      (defun cb-haskell/maybe-init-liquid-haskell ()
        (when (and cb-haskell/use-liquid-haskell? (executable-find "liquid"))
          (require 'flycheck-liquid)
          (require 'liquid-tip)
          (flycheck-add-next-checker 'haskell-ghc 'haskell-hlint)
          ;; (flycheck-add-next-checker 'haskell-hlint 'haskell-liquid)
          ;;(flycheck-select-checker 'haskell-liquid)
          ))

      (add-hook 'haskell-mode-hook #'cb-haskell/maybe-init-liquid-haskell)
      (add-hook 'literate-haskell-mode-hook #'cb-haskell/maybe-init-liquid-haskell))))

(defun cb-haskell/post-init-cb-buffers ()
  (use-package cb-buffers
    :config
    (add-to-list 'cb-buffers-indent-commands-alist '(haskell-mode . haskell/format-dwim))))
