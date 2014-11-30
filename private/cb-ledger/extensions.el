(defvar cb-ledger-pre-extensions
  '(
    ;; pre extension cb-ledgers go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-ledger-post-extensions
  '(
    ;; post extension cb-ledgers go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-ledger/init-<extension-cb-ledger>
;;
;; (defun cb-ledger/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
