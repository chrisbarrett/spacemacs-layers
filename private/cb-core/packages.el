(defvar cb-core-packages
  '(
    ;; package cores go here
    dash
    dash-functional
    s
    f
    noflet
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-core-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-core/init-<package-core>
;;
;; (defun cb-core/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
