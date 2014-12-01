(defvar cb-spelling-pre-extensions
  '(
    ;; pre extension cb-spellings go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-spelling-post-extensions
  '(
    ;; post extension cb-spellings go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-spelling/init-<extension-cb-spelling>
;;
;; (defun cb-spelling/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
