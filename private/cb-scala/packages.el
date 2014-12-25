(defvar cb-scala-packages
  '(
    ;; package cb-scalas go here
    scala-mode2
    sbt-mode
    ensime
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-scala-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-scala/init-<package-cb-scala>
;;
;; (defun cb-scala/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-scala/init-scala-mode2 ()
  (use-package scala-mode2
    :defer t
    :init
    (--each '(".cfe" ".cfs" ".si" ".gen" ".lock")
      (add-to-list 'completion-ignored-extensions it))
    :config
    (progn
      (setq scala-indent:align-forms t)
      (setq scala-indent:align-parameters t)
      (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)

      ;; Var face
      (set-face-foreground scala-font-lock:var-face solarized-hl-orange)
      (set-face-underline scala-font-lock:var-face nil)

      (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
        "Keep trailing-whitespace when indenting."
        (noflet ((scala-lib:delete-trailing-whitespace ()))
          ad-do-it))

      (add-hook 'scala-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'scala))))))

(defun cb-scala/init-ensime ()
  (use-package ensime
    :defer t
    :commands ensime-mode
    :init
    (add-hook 'scala-mode-hook 'ensime-mode)))
