(defvar cb-recentf-pre-extensions
  '(
    ;; pre extension cb-recentfs go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-recentf-post-extensions
  '(
    ;; post extension cb-recentfs go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-recentf/init-<extension-cb-recentf>
;;
;; (defun cb-recentf/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
