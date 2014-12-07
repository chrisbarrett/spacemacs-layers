(defvar cb-haskell-pre-extensions
  '(
    ;; pre extension cb-haskells go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-haskell-post-extensions
  '(
    ;; post extension cb-haskells go here
    super-smart-ops
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-haskell/init-<extension-cb-haskell>
;;
;; (defun cb-haskell/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-haskell/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'haskell-mode
        :add '("$" "=")
        :custom
        '(("." . haskell/smart-dot)
          ("," . haskell/smart-comma)
          ("|" . haskell/smart-pipe)
          ("#" . haskell/smart-hash)
          ("-" . haskell/smart-minus)
          (":" . haskell/smart-colon)
          (";" . haskell/smart-semicolon)))

      (super-smart-ops-configure-for-mode 'haskell-interactive-mode
        :add '("$" "=")
        :custom
        '(("." . haskell/smart-dot)
          ("-" . haskell/smart-minus)
          ("#" . haskell/smart-hash)
          ("|" . haskell/smart-pipe)
          (":" . haskell/ghci-smart-colon)
          ("," . haskell/ghci-smart-comma)
          (";" . haskell/smart-semicolon))))))
