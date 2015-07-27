;;; packages.el --- cb-scala Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-scala-packages
  '(
    scala-mode2
    sbt-mode
    ensime
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-scala-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-scala/init-scala-mode2 ()
  (use-package scala-mode2
    :defer t
    :mode (("/conf/routes$" . conf-mode))
    :config
    (progn
      (defun cb-scala/set-local-hooks ()
        (add-hook 'evil-insert-state-exit-hook 'scala/unicode-buffer))

      (add-hook 'scala-mode-hook 'cb-scala/set-local-hooks)

      (custom-set-faces
       `(scala-font-lock:var-face
         ((t (:foreground ,solarized-hl-orange :underline nil))))))))

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
      (setq ensime-auto-generate-config t)
      (setq ensime-prefer-noninteractive t)
      (setq ensime-sem-high-faces
            `((var . scala-font-lock:var-face)
              ;; (val . (:inherit font-lock-constant-face :slant italic))
              ;; (varField . scala-font-lock:var-face)
              ;; (valField . (:inherit font-lock-constant-face :slant italic))
              ;; (functionCall . font-lock-function-name-face)
              (operator . font-lock-keyword-face)
              (param . (:slant italic))
              (class . font-lock-type-face)
              (trait .  (:inherit font-lock-type-face :slant italic))
              (object . font-lock-constant-face)
              (package . font-lock-preprocessor-face)
              ))

      (defconst scala/test-file-template
        "package %TESTPACKAGE%

import org.scalatest.{ WordSpec, Matchers }

class %TESTCLASS% extends WordSpec with Matchers {

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
                  :test-template-fn (lambda () scala/test-file-template)))


      ;;; Fix up ensime files before loading
      (defadvice ensime-config-load (before fix-ensime-file activate)
        (scala/fix-ensime-file)))))

(defun cb-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    (progn
      (setq sbt:prompt-regexp (rx bol (group (? space) (or
                                                        ;; SBT
                                                        (and (+ (any alpha "-")) ">" (+ space))
                                                        ;; Activator
                                                        (and "[" (+ (any alpha "-")) "] $" (+ space)))
                                             ) (* space)))
      (setq sbt:program-name "sbt -Dsbt.log.noformat=true")
      (add-hook 'sbt-mode-hook (lambda () (aggressive-indent-mode -1)))
      (add-hook 'sbt-mode-hook 'turn-off-show-smartparens-mode)
      (add-hook 'sbt-mode-hook (lambda () (show-paren-mode -1))))))
