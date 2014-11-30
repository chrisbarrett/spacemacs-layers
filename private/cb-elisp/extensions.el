(defvar cb-elisp-pre-extensions
  '(
    ;; pre extension elisps go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-elisp-post-extensions
  '(
    ;; post extension elisps go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function elisp/init-<extension-elisp>
;;
;; (defun elisp/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
