;;; packages.el --- cb-csharp Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-csharp-packages
  '(omnisharp)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-csharp-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-csharp/init-omnisharp ()
  (use-package omnisharp
    :commands omnisharp-mode
    :diminish "O#"
    :config
    (progn
      (add-hook 'csharp-mode-hook 'omnisharp-mode)
      (add-hook 'csharp-mode-hook 'eldoc-mode-hook)
      (setq omnisharp-server-executable-path "~/src/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")
      (setq omnisharp-auto-complete-want-documentation nil))))
