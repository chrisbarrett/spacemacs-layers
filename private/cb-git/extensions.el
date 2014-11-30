(defvar cb-git-pre-extensions
  '(
    ;; pre extension cb-gits go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-git-post-extensions
  '(
    ;; post extension cb-gits go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-git/init-<extension-cb-git>
;;
;; (defun cb-git/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
