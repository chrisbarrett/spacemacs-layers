;;; packages.el --- cb-rust Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-rust-packages
  '(
    rust-mode
    flycheck-rust
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-rust-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-rust/init-rust-mode ()
  (use-package rust-mode
    :mode "\\.rs\\'"
    :config
    (add-hook 'rust-mode-hook 'rust/set-rust-library-path)))

(defun cb-rust/init-flycheck-rust ()
  (use-package flycheck-rust
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
    :config
    (put 'rust :flycheck-command
         '("rustc" "--crate-type" "lib" "--no-trans"
           (option-list "-L" flycheck-rust-library-path s-prepend)
           source-inplace))))
