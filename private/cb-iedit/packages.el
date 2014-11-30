(defvar cb-iedit-packages
  '(
    ;; package cb-iedits go here
    iedit
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-iedit-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-iedit/init-<package-cb-iedit>
;;
;; (defun cb-iedit/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-iedit/init-iedit ()
  (use-package iedit
    :config
    (custom-set-faces
     `(iedit-occurrence ((t (:background ,solarized-hl-orange :foreground "white")))))))
