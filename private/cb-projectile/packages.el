(defvar cb-projectile-packages
  '(
    ;; package cb-projectiles go here
    ack-and-a-half
    projectile
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-projectile-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-projectile/init-<package-cb-projectile>
;;
;; (defun cb-projectile/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
