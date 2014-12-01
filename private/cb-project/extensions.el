(defvar cb-project-pre-extensions
  '(
    ;; pre extension cb-projects go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-project-post-extensions
  '(
    ;; post extension cb-projects go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-project/init-<extension-cb-project>
;;
;; (defun cb-project/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
