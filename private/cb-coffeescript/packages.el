(defvar cb-coffeescript-packages
  '(
    ;; package cb-coffeescripts go here
    coffee-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-coffeescript-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-coffeescript/init-<package-cb-coffeescript>
;;
;; (defun cb-coffeescript/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-coffeescript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t))
