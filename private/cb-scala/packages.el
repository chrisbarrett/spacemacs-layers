(defvar cb-scala-packages
  '(
    ;; package cb-scalas go here
    scala-mode2
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
    :config
    (progn
      (setq scala-indent:align-forms t)
      (setq scala-indent:align-parameters t)
      (setq scala-indent:default-run-on-strategy scala-indent:eager-strategy))))

(defun cb-scala/init-ensime ()
  (use-package ensime
    :defer t
    :init
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))
