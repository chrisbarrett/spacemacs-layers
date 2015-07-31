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
      (custom-set-faces
       `(scala-font-lock:var-face
         ((t (:foreground ,solarized-hl-orange :underline nil)))))

      (defun cb-scala/set-local-hooks ()
        (add-hook 'evil-insert-state-exit-hook 'scala/unicode-buffer))

      (add-hook 'scala-mode-hook 'cb-scala/set-local-hooks)

      (define-key scala-mode-map (kbd "M-RET") 'scala/meta-ret)
      (define-key scala-mode-map (kbd "C-c C-e") 'scala/insert-extends)

      ;; HACK: set some scala key bindings in a hook to prevent them mysteriously
      ;; leaking into other major modes.

      (defun scala/set-normal-state-local-keybindings ()
        (evil-local-set-key 'insert (kbd "<return>") 'scala/ret)
        (evil-local-set-key 'normal (kbd "RET") 'ensime-inspect-type-at-point))

      (add-hook 'scala-mode-hook 'scala/set-normal-state-local-keybindings)
      )))

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
        (scala/fix-ensime-file))

      (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src)
      (define-key ensime-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
      (define-key ensime-mode-map (kbd "C-c C-l") 'scala/load-buffer)
      (define-key ensime-mode-map (kbd "C-c C-h") 'ensime-show-doc-for-symbol-at-point)
      (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
      (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)

      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "M-.") 'ensime-inspector-browse-source
        (kbd "K") 'ensime-inspector-browse-doc
        (kbd ",") 'ensime-inspector-backward-page
        (kbd ".") 'ensime-inspector-forward-page
        (kbd "^") 'ensime-inspector-backward-page)

      (evil-define-key 'normal ensime-mode-map (kbd "M-N") 'ensime-forward-note)
      (evil-define-key 'normal ensime-mode-map (kbd "M-P") 'ensime-backward-note)
      (evil-define-key 'normal ensime-mode-map (kbd "RET") 'ensime-inspect-type-at-point)

      (evil-leader/set-key-for-mode 'scala-mode "ii" 'ensime-import-type-at-point)
      )))

(defun cb-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    (progn
      (setq sbt:prompt-regexp
            (rx bol (group (? space) (or
                                      ;; SBT
                                      (and (+ (any alpha "-")) ">" (+ space))
                                      ;; Activator
                                      (and "[" (+ (any alpha "-")) "] $" (+ space)))
                           ) (* space)))
      (setq sbt:program-name "sbt -Dsbt.log.noformat=true")

      (defun cb-scala/set-up-sbt-mode ()
        (aggressive-indent-mode -1)
        (show-smartparens-mode -1)
        (show-paren-mode -1)
        (local-set-key (kbd "C-l") 'comint-clear-buffer)
        (local-set-key (kbd "C-c RET") 'scala/sbt-send-ret))

      (add-hook 'sbt-mode-hook 'cb-scala/set-up-sbt-mode))))
