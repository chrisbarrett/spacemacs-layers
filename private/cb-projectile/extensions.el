(defvar cb-projectile-pre-extensions
  '(
    ;; pre extension cb-projectiles go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-projectile-post-extensions
  '(
    ;; post extension cb-projectiles go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-projectile/init-<extension-cb-projectile>
;;
;; (defun cb-projectile/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
