(defvar cb-rust-pre-extensions
  '(
    ;; pre extension cb-rusts go here
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-rust-post-extensions
  '(
    ;; post extension cb-rusts go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-rust/init-<extension-cb-rust>
;;
;; (defun cb-rust/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-rust/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'rust-mode
      :rem '("!" "~" "&")
      :custom '((":" . rust/smart-colon)
                ("," . core/comma-then-space)))))
