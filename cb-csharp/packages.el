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
  (setq omnisharp-auto-complete-want-documentation nil))

(defun cb-csharp/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'csharp-mode
      (smart-ops "," ";" :pad-before nil :pad-after t)
      (smart-ops-default-ops))))
