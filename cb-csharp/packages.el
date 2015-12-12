;;; packages.el --- cb-csharp Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-csharp-packages
  '(omnisharp
    smart-ops))

(defun cb-csharp/post-init-omnisharp ()
  (diminish 'omnisharp-mode "O#")
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'eldoc-mode)
  (setq omnisharp-server-executable-path "~/src/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")
  (setq omnisharp-auto-complete-want-documentation nil)

  (with-eval-after-load 'omnisharp
    (evil-leader/set-key-for-mode 'csharp-mode " ii" 'csharp/insert-using)

    (define-key csharp-mode-map (kbd "C-c C-c") 'omnisharp-fix-code-issue-at-point)
    (define-key csharp-mode-map (kbd "C-c C-t") 'omnisharp-current-type-information)
    (define-key csharp-mode-map (kbd "C-c C-h") 'omnisharp-current-type-documentation)

    (evil-define-key 'insert csharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
    (evil-define-key 'insert csharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
    (evil-define-key 'normal csharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)

    (evil-leader/set-key-for-mode 'csharp-mode "ou"  'omnisharp-find-usages)
    (evil-leader/set-key-for-mode 'csharp-mode "oi" 'omnisharp-find-implementations) ; g i is taken
    (evil-leader/set-key-for-mode 'csharp-mode "or" 'omnisharp-run-code-action-refactoring)
    (evil-leader/set-key-for-mode 'csharp-mode "oF" 'omnisharp-fix-usings)
    (evil-leader/set-key-for-mode 'csharp-mode "oR" 'omnisharp-rename)
    (evil-leader/set-key-for-mode 'csharp-mode "oor" 'omnisharp-navigate-to-current-file-member)
    (evil-leader/set-key-for-mode 'csharp-mode "oos" 'omnisharp-navigate-to-solution-member)
    (evil-leader/set-key-for-mode 'csharp-mode "oof" 'omnisharp-navigate-to-solution-file-then-file-member)
    (evil-leader/set-key-for-mode 'csharp-mode "o." 'omnisharp-show-overloads-at-point)
    (evil-leader/set-key-for-mode 'csharp-mode "oc" 'recompile)

    (evil-leader/set-key-for-mode 'csharp-mode "oots"
      (lambda () (interactive) (omnisharp-unit-test "single")))

    (evil-leader/set-key-for-mode 'csharp-mode "ootf"
      (lambda () (interactive) (omnisharp-unit-test "fixture")))

    (evil-leader/set-key-for-mode 'csharp-mode "oott"
      (lambda () (interactive) (omnisharp-unit-test "all")))))

(defun cb-csharp/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'csharp-mode
      (smart-ops "," ";" :pad-before nil :pad-after t)
      (smart-ops-default-ops))))
