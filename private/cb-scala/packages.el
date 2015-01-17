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
    :config
    (progn
      ;; Var face
      (set-face-foreground scala-font-lock:var-face solarized-hl-orange)
      (set-face-underline scala-font-lock:var-face nil))))

(defun cb-scala/init-ensime ()
  (use-package ensime
    :defer t
    :init
    (progn
      (add-hook 'scala-mode-hook 'scala/maybe-start-ensime)
      (add-hook 'ensime-mode-hook (lambda () (aggressive-indent-mode -1))))))
