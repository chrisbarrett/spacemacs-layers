(defvar cb-csharp-pre-extensions
  '(
    ;; pre extension cb-csharps go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-csharp-post-extensions
  '(
    ;; post extension cb-csharps go here
    super-smart-ops
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-csharp/init-<extension-cb-csharp>
;;
;; (defun cb-csharp/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package


(defun cb-csharp/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'csharp-mode)))
