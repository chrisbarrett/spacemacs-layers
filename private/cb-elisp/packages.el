(defvar cb-elisp-packages
  '(
    ;; package elisps go here
    elisp-slime-nav
    cl-lib-highlight
    paredit
    eval-sexp-fu
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-elisp-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function elisp/init-<package-elisp>
;;
;; (defun elisp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
