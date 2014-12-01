(defvar cb-core-pre-extensions
  '(
    ;; pre extension cores go here
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-core-post-extensions
  '(
    ;; post extension cores go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function core/init-<extension-core>
;;
;; (defun core/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun core/init-super-smart-ops ()
  (use-package super-smart-ops))
