(defvar cb-coffeescript-pre-extensions
  '(
    ;; pre extension cb-coffeescripts go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-coffeescript-post-extensions
  '(
    ;; post extension cb-coffeescripts go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-coffeescript/init-<extension-cb-coffeescript>
;;
;; (defun cb-coffeescript/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
