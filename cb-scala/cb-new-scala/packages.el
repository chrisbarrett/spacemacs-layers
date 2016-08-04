;;; packages.el --- cb-new-scala layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Barrett <chris.d.barrett@me.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-new-scala-packages
  '(ensime
    flycheck
    aggressive-indent
    scala-mode
    sbt-mode
    (cb-scala-flyspell :location local)
    (cb-scala-join-line :location local)
    (cb-scala-smart-ops :location local)
    (cb-scala-yasnippet :location local)
    (cb-scala-autoinsert :location local)
    (cb-scala-eldoc :location local)
    (cb-scala-ret :location local)
    (ensime-flycheck-integration :location local)
    (ensime-diminished-modeline :location local)
    (sbt-file-mode :location local)
    (cb-scala-ligatures :location local)))

(defun cb-new-scala/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'sbt-file-mode))))

(defun cb-new-scala/post-init-flycheck ()
  (use-package flycheck
    :init
    (spacemacs/add-flycheck-hook 'scala-mode)
    :config
    (progn
      (setq flycheck-scalastylerc "~/.scalastyle.xml")

      (defun cb-new-scala/disable-flycheck-scala ()
        (when (boundp 'flycheck-disabled-checkers)
          (push 'scala flycheck-disabled-checkers)))
      (add-hook 'ensime-mode-hook #'cb-new-scala/disable-flycheck-scala))))

(defconst cb-new-scala/test-file-template
  "import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpec }

class %TESTCLASS% extends WordSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  override def beforeAll() = {
  }

  after {
  }

  \"\" should {
    \"\" in pending
  }

}
"
  "The default value to insert into new scala test buffers.
See `ensime-goto-test-config-defaults' for possible template values.")

(defun cb-new-scala/init-ensime ()
  (use-package ensime
    :after scala-mode

    :init
    (progn
      (dolist (prefix '(("mb" . "scala/build")
                        ("mc" . "scala/check")
                        ("md" . "scala/debug")
                        ("me" . "scala/errors")
                        ("mg" . "scala/goto")
                        ("mh" . "scala/docs")
                        ("mi" . "scala/inspect")
                        ("mn" . "scala/ensime")
                        ("mr" . "scala/refactor")
                        ("mt" . "scala/test")
                        ("ms" . "scala/repl")
                        ("my" . "scala/yank")))
        (spacemacs/declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix)))

      (defun cb-new-scala/ensime-refactor-accept ()
        (interactive)
        (with-no-warnings (funcall continue-refactor))
        (ensime-popup-buffer-quit-function))

      (defun cb-new-scala/ensime-refactor-cancel ()
        (interactive)
        (with-no-warnings (funcall cancel-refactor))
        (ensime-popup-buffer-quit-function))


      (defun cb-new-scala/ensime-gen-and-restart()
        "Regenerate `.ensime' file and restart the ensime server."
        (interactive)
        (progn
          (sbt-command ";ensimeConfig;ensimeConfigProject")
          (ensime-shutdown)
          (ensime)))

      (defun cb-new-scala/ensime-inf-eval-buffer-switch ()
        "Send buffer content to shell and switch to it in insert mode."
        (interactive)
        (ensime-inf-eval-buffer)
        (ensime-inf-switch)
        (evil-insert-state))

      (defun cb-new-scala/ensime-inf-eval-region-switch (start end)
        "Send region content to shell and switch to it in insert mode."
        (interactive "r")
        (ensime-inf-switch)
        (ensime-inf-eval-region start end)
        (evil-insert-state)))

    :evil-bind
    (:map
     ensime-mode-map
     :state insert
     ("M-." . ensime-edit-definition)
     ("M-," . ensime-pop-find-definition-stack)
     :state normal
     ("M-." . ensime-edit-definition)
     ("M-," . ensime-pop-find-definition-stack)
     ("RET" . ensime-inspect-type-at-point)

     :map ensime-popup-buffer-map
     :state normal
     ("q" . ensime-popup-buffer-quit-function)

     :map ensime-inspector-mode-map
     :state normal
     ("M-." . ensime-inspector-browse-source)
     ("K" . ensime-inspector-browse-doc)
     ("q" . ensime-popup-buffer-quit-function)
     ("," . ensime-inspector-backward-page)
     ("." . ensime-inspector-forward-page)
     ("^" . ensime-inspector-backward-page)

     :map ensime-refactor-info-map
     :state normal
     ("q" . cb-new-scala/ensime-refactor-cancel)
     ("c" . cb-new-scala/ensime-refactor-accept)
     ("RET" . cb-new-scala/ensime-refactor-accept)

     :map ensime-compile-result-map
     :state normal
     ("g" . ensime-show-all-errors-and-warnings)
     ("TAB" . forward-button)
     ("<backtab>" . backward-button)
     ("M-n" . forward-button)
     ("M-p" . backward-button)
     ("n" . forward-button)
     ("N" . backward-button))

    :leader-bind
    (:mode scala-mode
           ("/" . ensime-search)
           ("'" . ensime-inf-switch)

           ("bc" . ensime-sbt-do-compile)
           ("bC" . ensime-sbt-do-clean)
           ("bi" . ensime-sbt-switch)
           ("bp" . ensime-sbt-do-package)
           ("br" . ensime-sbt-do-run)

           ("ct" . ensime-typecheck-current-buffer)
           ("cT" . ensime-typecheck-all)

           ("ee" . ensime-print-errors-at-point)
           ("el" . ensime-show-all-errors-and-warnings)
           ("es" . ensime-stacktrace-switch)

           ("gg" . ensime-edit-definition)
           ("gp" . ensime-pop-find-definition-stack)
           ("gi" . ensime-goto-impl)
           ("gt" . ensime-goto-test)

           ("hh" . ensime-show-doc-for-symbol-at-point)
           ("hT" . ensime-type-at-point-full-name)
           ("ht" . ensime-type-at-point)
           ("hu" . ensime-show-uses-of-symbol-at-point)

           ("ii" . ensime-import-type-at-point)
           ("iI" . ensime-inspect-type-at-point-other-frame)
           ("ip" . ensime-inspect-project-package)

           ("nF" . ensime-reload-open-files)
           ("ns" . ensime)
           ("nS" . cb-new-scala/ensime-gen-and-restart)

           ("ra" . ensime-refactor-add-type-annotation)
           ("rd" . ensime-refactor-diff-inline-local)
           ("rD" . ensime-undo-peek)
           ("rf" . ensime-format-source)
           ("ri" . ensime-refactor-diff-organize-imports)
           ("rm" . ensime-refactor-diff-extract-method)
           ("rr" . ensime-refactor-diff-rename)
           ("rt" . ensime-import-type-at-point)
           ("rv" . ensime-refactor-diff-extract-local)

           ("ta" . ensime-sbt-do-test-dwim)
           ("tr" . ensime-sbt-do-test-quick-dwim)
           ("tt" . ensime-sbt-do-test-only-dwim)

           ("sa" . ensime-inf-load-file)
           ("sb" . ensime-inf-eval-buffer)
           ("sB" . cb-new-scala/ensime-inf-eval-buffer-switch)
           ("si" . ensime-inf-switch)
           ("sr" . ensime-inf-eval-region)
           ("sR" . cb-new-scala/ensime-inf-eval-region-switch)

           ("z" . ensime-expand-selection-command))

    :config
    (progn
      (setq ensime-startup-snapshot-notification nil)
      (setq ensime-auto-generate-config t)
      (setq ensime-implicit-gutter-icons nil)
      (setq ensime-startup-dirname (f-join spacemacs-cache-directory "ensime"))
      (setq ensime-sem-high-faces
            `((var . scala-font-lock:var-face)
              ;; (val . (:inherit font-lock-constant-face :slant italic))
              (varField . scala-font-lock:var-face)
              ;; (valField . (:inherit font-lock-constant-face :slant italic))
              (functionCall . font-lock-function-name-face)
              (operator . font-lock-keyword-face)
              (param . (:slant italic))
              (class . font-lock-type-face)
              (trait .  (:inherit font-lock-type-face :slant italic))
              (object . font-lock-constant-face)
              (package . font-lock-preprocessor-face)
              (implicitConversion . (:underline ,(with-no-warnings cb-vars-solarized-hl-cyan)))
              (implicitParams . (:underline ,(with-no-warnings cb-vars-solarized-hl-cyan)))
              (deprecated . (:strike-through "dark gray"))))

      (setq ensime-goto-test-config-defaults
            (list :test-class-names-fn #'ensime-goto-test--test-class-names
                  :test-class-suffixes '("Test" "Tests"
                                         "IntTest" "IntTests" "IntegrationTest" "IntegrationTests"
                                         "Spec" "Specs" "Specification" "Specifications"
                                         "Prop" "Props" "Property" "Properties"
                                         "Check" "Checks")
                  :impl-class-name-fn #'ensime-goto-test--impl-class-name
                  :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
                  :is-test-dir-fn #'ensime-goto-test--is-test-dir
                  :test-template-fn (lambda () cb-new-scala/test-file-template)))

      (spacemacs/register-repl 'ensime #'ensime-inf-switch "ensime")))

  (use-package ensime-expand-region
    :after ensime)

  (use-package ensime-company
    :after ensime
    :config
    (progn
      ;; HACK: Prevent ensime from clobbering company settings.
      (with-eval-after-load 'ensime-company
        (defun ensime-company-enable ()
          (set (make-local-variable 'company-backends) '(ensime-company))
          (company-mode)
          (yas-minor-mode-on)
          (set (make-local-variable 'company-idle-delay) 0))))))

(defun cb-new-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :leader-bind (:mode scala-mode ("bb" . sbt-command))
    :config
    (progn
      (setq sbt:program-name "sbt -Dsbt.log.noformat=true")
      (setq sbt:prompt-regexp
            (rx bol
                (group (? space)
                       (or
                        ;; SBT
                        (and (+ (any alpha "-")) ">" (+ space))
                        ;; Activator
                        (and "[" (+ (any alpha "-")) "] $" (+ space))))
                (* space)))

      (defun cb-new-scala/sbt-send-ret ()
        "Send the current REPL contents to SBT."
        (interactive)
        (process-send-string (get-buffer-process (current-buffer)) "\n")
        (goto-char (point-max))
        (evil-insert-state))

      (defun cb-new-scala/set-up-sbt-mode ()
        (aggressive-indent-mode -1)
        (show-smartparens-mode -1)
        (show-paren-mode -1)
        (local-set-key (kbd "C-l") #'spacemacs/comint-clear-buffer)
        (local-set-key (kbd "C-c RET") #'cb-new-scala/sbt-send-ret))

      (add-hook 'sbt-mode-hook #'cb-new-scala/set-up-sbt-mode))))

(defun cb-new-scala/init-scala-mode ()
  (use-package scala-mode
    :mode ("\\.scala\\'" . scala-mode)
    :init
    (add-to-list 'auto-mode-alist '("/conf/routes\\'" . conf-mode))
    :config
    (progn
      (setq scala-indent:align-forms t)
      (setq scala-indent:align-parameters t)
      (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)

      (custom-set-faces
       `(scala-font-lock:var-face
         ((t (:foreground ,(with-no-warnings cb-vars-solarized-hl-orange) :underline nil))))))))

(defun cb-new-scala/init-cb-scala-flyspell ()
  (use-package cb-scala-flyspell
    :after scala-mode
    :config (cb-scala-flyspell-init)))

(defun cb-new-scala/init-cb-scala-join-line ()
  (use-package cb-scala-join-line
    :after scala-mode
    :evil-bind
    (:map scala-mode-map :state normal ("J" . cb-scala-join-line))))

(defun cb-new-scala/init-cb-scala-smart-ops ()
  (use-package cb-scala-smart-ops
    :after scala-mode
    :config (cb-scala-smart-ops-init)))

(defun cb-new-scala/init-cb-scala-yasnippet ()
  (use-package cb-scala-yasnippet
    :after scala-mode
    :config (cb-scala-yasnippet-init)))

(defun cb-new-scala/init-cb-scala-autoinsert ()
  (use-package cb-scala-autoinsert
    :config (cb-scala-autoinsert-init)))

(defun cb-new-scala/init-cb-scala-eldoc ()
  (use-package cb-scala-eldoc
    :after scala-mode
    :config (cb-scala-eldoc-init)))

(defun cb-new-scala/init-cb-scala-ret ()
  (use-package cb-scala-ret
    :after scala-mode
    :evil-bind
    (:map
     scala-mode-map
     :state normal
     ("M-RET" . cb-scala-meta-ret)
     ("RET" . cb-scala-ret)

     :state insert
     ("M-RET" . cb-scala-meta-ret)
     ("RET" . cb-scala-ret))))

(defun cb-new-scala/init-ensime-flycheck-integration ()
  (use-package ensime-flycheck-integration
    :after ensime
    :config (ensime-flycheck-integration-init)))

(defun cb-new-scala/init-ensime-diminished-modeline ()
  (use-package ensime-diminished-modeline
    :after ensime))

(defun cb-new-scala/post-init-indent-dwim ()
  (use-package indent-dwim
    :after ensime
    :config
    (add-to-list 'indent-dwim-commands-alist '(scala-mode . ensime-format-source))))

(defun cb-new-scala/init-sbt-file-mode ()
  (use-package sbt-file-mode
    :mode ("\\.sbt\\'" . sbt-file-mode)))

(defun cb-new-scala/init-cb-scala-ligatures ()
  (use-package cb-scala-ligatures
    :after scala-mode
    :config (cb-scala-ligatures-init)))

;;; packages.el ends here
