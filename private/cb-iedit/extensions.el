(defvar cb-iedit-pre-extensions
  '(
    ;; pre extension cb-iedits go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-iedit-post-extensions
  '(
    ;; post extension cb-iedits go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-iedit/init-<extension-cb-iedit>
;;
;; (defun cb-iedit/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
