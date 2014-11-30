(defvar cb-smartparens-pre-extensions
  '(
    ;; pre extension cb-smartparenss go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-smartparens-post-extensions
  '(
    ;; post extension cb-smartparenss go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-smartparens/init-<extension-cb-smartparens>
;;
;; (defun cb-smartparens/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
