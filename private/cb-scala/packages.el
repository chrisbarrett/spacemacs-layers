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
      (setq ensime-default-scala-version "2.11.5")
      (add-hook 'scala-mode-hook 'scala/maybe-start-ensime)
      (add-hook 'ensime-mode-hook (lambda () (aggressive-indent-mode -1))))
    :config
    (progn

      (defconst scala/test-file-template
        "package %TESTPACKAGE%

import org.scalatest.{ FunSpec, Matchers }

class %TESTCLASS% extends FunSpec with Matchers {

}
"
        "The default value to insert into new scala test buffers.
See `ensime-goto-test-config-defaults' for possible template values.")

      (setq ensime-goto-test-config-defaults
            (list :test-class-names-fn 'ensime-goto-test--test-class-names
                  :test-class-suffixes '("Test" "Tests"
                                         "IntTest" "IntTests" "IntegrationTest" "IntegrationTests"
                                         "Spec" "Specs" "Specification" "Specifications"
                                         "Prop" "Props" "Property" "Properties"
                                         "Check" "Checks")
                  :impl-class-name-fn 'ensime-goto-test--impl-class-name
                  :impl-to-test-dir-fn 'ensime-goto-test--impl-to-test-dir
                  :is-test-dir-fn 'ensime-goto-test--is-test-dir
                  :test-template-fn (lambda () scala/test-file-template))))))

(defun cb-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init
    (progn
      (setq sbt:prompt-regexp (rx bol (group (? space) (* alpha) ">" (+ space)) (* space)))
      (setq sbt:program-name "sbt -Dsbt.log.noformat=true"))))
