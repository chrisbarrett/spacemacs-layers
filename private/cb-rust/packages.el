(defvar cb-rust-packages
  '(
    ;; package cb-rusts go here
    rust-mode
    flycheck-rust
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-rust-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-rust/init-<package-cb-rust>
;;
;; (defun cb-rust/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

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
