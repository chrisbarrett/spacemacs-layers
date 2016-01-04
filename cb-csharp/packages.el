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
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode " ii" 'csharp/insert-using)

    (define-key csharp-mode-map (kbd "C-c C-c") 'omnisharp-fix-code-issue-at-point)
    (define-key csharp-mode-map (kbd "C-c C-t") 'omnisharp-current-type-information)
    (define-key csharp-mode-map (kbd "C-c C-h") 'omnisharp-current-type-documentation)

    (evil-define-key 'insert csharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
    (evil-define-key 'insert csharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
    (evil-define-key 'normal csharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)

    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "ou"  'omnisharp-find-usages)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oi" 'omnisharp-find-implementations) ; g i is taken
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "or" 'omnisharp-run-code-action-refactoring)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oF" 'omnisharp-fix-usings)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oR" 'omnisharp-rename)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oor" 'omnisharp-navigate-to-current-file-member)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oos" 'omnisharp-navigate-to-solution-member)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oof" 'omnisharp-navigate-to-solution-file-then-file-member)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "o." 'omnisharp-show-overloads-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oc" 'recompile)

    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oots"
      (lambda () (interactive) (omnisharp-unit-test "single")))

    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "ootf"
      (lambda () (interactive) (omnisharp-unit-test "fixture")))

    (spacemacs/set-leader-keys-for-major-mode 'csharp-mode "oott"
      (lambda () (interactive) (omnisharp-unit-test "all")))))

(defun cb-csharp/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'csharp-mode
      (smart-ops "," ";" :pad-before nil :pad-after t)
      (smart-ops-default-ops))))
