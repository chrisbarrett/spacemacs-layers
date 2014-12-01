(defvar cb-proof-packages
  '(
    ;; package cb-proofs go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-proof-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-proof/init-<package-cb-proof>
;;
;; (defun cb-proof/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
