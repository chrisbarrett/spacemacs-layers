(defvar cb-scala-pre-extensions
  '(
    ;; pre extension cb-scalas go here
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-scala-post-extensions
  '(
    ;; post extension cb-scalas go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-scala/init-<extension-cb-scala>
;;
;; (defun cb-scala/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-scala/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'scala-mode
        :custom
        '(("=" . scala/equals)
          (":" . scala/colon)
          ("+" . scala/plus)
          ("-" . scala/minus)
          ("," . core/comma-then-space)))

      (super-smart-ops-configure-for-mode 'sbt-mode
        :custom
        '(("=" . scala/equals)
          (":" . scala/repl-colon)
          ("+" . scala/plus)
          ("-" . scala/minus)
          ("," . core/comma-then-space)))

      (super-smart-ops-configure-for-mode 'ensime-inf-mode
        :custom
        '(("=" . scala/equals)
          (":" . scala/repl-colon)
          ("+" . scala/plus)
          ("-" . scala/minus)
          ("," . core/comma-then-space))))))
