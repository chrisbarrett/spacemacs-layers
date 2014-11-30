(defvar cb-cosmetic-pre-extensions
  '(
    ;; pre extension cb-cosmetics go here
    lambda-mode
    )
  "List of all extensions to load before the packages.")

(defvar cb-cosmetic-post-extensions
  '(
    ;; post extension cb-cosmetics go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-cosmetic/init-<extension-cb-paren-face>
;;
;; (defun cb-cosmetic/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
