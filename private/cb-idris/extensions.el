(defvar cb-idris-pre-extensions
  '(
    ;; pre extension cb-idriss go here
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-idris-post-extensions
  '(
    ;; post extension cb-idriss go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-idris/init-<extension-cb-idris>
;;
;; (defun cb-idris/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-idris/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'idris-mode
        :add '("$")
        :custom
        '(("?" . idris/smart-question-mark)
          ("|" . idris/smart-pipe)
          ("." . idris/smart-dot)
          ("," . idris/smart-comma)
          (":" . idris/smart-colon)))

      (super-smart-ops-configure-for-mode 'idris-repl-mode
        :add '("$")
        :custom
        '(("?" . idris/smart-question-mark)
          ("|" . idris/smart-pipe)
          ("." . idris/smart-dot)
          ("," . idris/smart-comma)
          (":" . idris/smart-colon))))))
