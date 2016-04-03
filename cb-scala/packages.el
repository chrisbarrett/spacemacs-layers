;;; packages.el --- cb-scala Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(require 'dash)
(require 's)
(require 'f)

(defconst cb-scala-packages
  '(scala-mode2
    sbt-mode
    ensime
    smart-ops
    indent-dwim
    aggressive-indent
    flycheck
    popwin

    (sbt-file-mode :location local)
    (scala-errors :location local)
    (scala-pretty-sbt :location local)
    (scala-yasnippet :location local)
    (ensime-flycheck-integration :location local)
    (ensime-diminished-modeline :location local)
    (scala-autoinsert :location local)))

(defun cb-scala/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'sbt-file-mode))))

(defun cb-scala/post-init-scala-mode2 ()
  (use-package scala-mode2
    :defer t
    :mode (("/conf/routes$" . conf-mode))
    :config
    (progn
      (custom-set-faces
       `(scala-font-lock:var-face
         ((t (:foreground ,solarized-hl-orange :underline nil)))))

      (defun cb-scala/set-local-values ()
        (add-hook 'evil-insert-state-exit-hook #'scala/unicode-buffer))

      (add-hook 'scala-mode-hook #'cb-scala/set-local-values)

      (define-key scala-mode-map (kbd "M-RET") #'scala/meta-ret)

      (define-key scala-mode-map (kbd "C-c C-e") #'scala/insert-extends)

      ;;; HACK: Spacemacs errors when trying to set up ensime for buffers
      ;;; without file names, such as ediff buffers.

      (defun cb-scala/ignore-errors (fn &rest _)
        (ignore-errors
          (funcall fn)))

      (advice-add #'scala/configure-ensime :around #'cb-scala/ignore-errors))))

(defun cb-scala/post-init-ensime ()

  (defun scala/configure-ensime ()
    "Ensure the file exists before starting `ensime-mode'."
    (unless (f-ext? (buffer-file-name) "sbt")
      (ensime-mode +1)))

  (add-hook 'scala-mode-hook #'scala/configure-ensime)
  (add-hook 'ensime-mode-hook #'cb-scala/turn-off-aggressive-indent)

  (setq ensime-auto-generate-config t)
  (setq ensime-implicit-gutter-icons nil)
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
          (implicitConversion . (:underline ,solarized-hl-cyan))
          (implicitParams . (:underline ,solarized-hl-cyan))
          (deprecated . (:strike-through "dark gray"))))

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


  ;; Fix up ensime files before loading
  (advice-add 'ensime-config-load :before #'scala/fix-ensime-file)

  (with-eval-after-load 'ensime-inf
    (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src))

  (with-eval-after-load 'ensime

    (define-key ensime-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
    (define-key ensime-mode-map (kbd "C-c C-l") 'scala/load-buffer)
    (define-key ensime-mode-map (kbd "C-c C-h") 'ensime-show-doc-for-symbol-at-point)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "M-.") 'ensime-inspector-browse-source
      (kbd "K") 'ensime-inspector-browse-doc
      (kbd ",") 'ensime-inspector-backward-page
      (kbd ".") 'ensime-inspector-forward-page
      (kbd "^") 'ensime-inspector-backward-page)

    (evil-define-key 'normal ensime-mode-map (kbd "RET") 'ensime-inspect-type-at-point))

  ;; HACK: Prevent ensime from clobbering company settings.
  (with-eval-after-load 'ensime-company
    (defun ensime-company-enable ()
      (set (make-local-variable 'company-backends) '(ensime-company))
      (company-mode)
      (yas-minor-mode-on)
      (set (make-local-variable 'company-idle-delay) 0)))

  ;; HACK: Fix errors with ensime eldoc function.
  (with-eval-after-load 'ensime-inspector
    (defun ensime-type-at-point (&optional arg)
      "Echo the type at point to the minibuffer.
A prefix argument will add the type to the kill ring."
      (interactive "P")
      (let* ((type (ensime-rpc-get-type-at-point))
             (fullname (ensime-type-full-name-with-args type)))
        (when arg
          (kill-new fullname))
        (message fullname))))

  (spacemacs/set-leader-keys-for-major-mode 'scala-mode "ii" 'ensime-import-type-at-point))

(defun cb-scala/post-init-sbt-mode ()
  (bind-key "<S-f1>" 'scala-sbt/bring)

  (setq sbt:prompt-regexp
        (rx bol (group (? space) (or
                                  ;; SBT
                                  (and (+ (any alpha "-")) ">" (+ space))
                                  ;; Activator
                                  (and "[" (+ (any alpha "-")) "] $" (+ space)))
                       ) (* space)))
  (setq sbt:program-name "sbt -Dsbt.log.noformat=true")

  (defun cb-scala/set-up-sbt-mode ()
    (cb-scala/turn-off-aggressive-indent)
    (show-smartparens-mode -1)
    (show-paren-mode -1)
    (local-set-key (kbd "C-l") #'spacemacs/comint-clear-buffer)
    (local-set-key (kbd "C-c RET") #'scala/sbt-send-ret))

  (add-hook 'sbt-mode-hook #'cb-scala/set-up-sbt-mode))

(defun cb-scala/post-init-smart-ops ()
  (use-package smart-ops
    :after scala-mode2
    :config
    (progn

      (defun scala/replace-slashes-with-doc ()
        (interactive "*")
        (atomic-change-group
          (delete-region (line-beginning-position) (line-end-position))
          (scala/insert-scaladoc)))

      (defun scala/insert-scaladoc ()
        "Insert the skeleton of a ScalaDoc at point."
        (interactive "*")
        (indent-for-tab-command) (insert "/**")
        (core/open-line-below-current-indentation) (insert " * ")
        (save-excursion
          (core/open-line-below-current-indentation) (insert "*/")))

      (defun cb-scala/at-repl-prompt? ()
        (s-matches? (rx bol (* space) "scala>" (* space))
                    (buffer-substring (line-beginning-position) (point))))

      (defconst cb-scala/common-ops
        (-flatten-n 1
                    (list
                     (smart-ops "???" "?" "=" "==" "+" "-" "*" "/" "<" ">" "|" "$" "&" "%" "!" "~")
                     (smart-ops ":" "," :pad-before nil)
                     (smart-ops "@" :pad-after nil)
                     (smart-ops ":=")

                     ;; Inserting this op means you're probably editing a
                     ;; function return type. Pad internally and move point
                     ;; inside.
                     (let ((inserting-type? (smart-ops-before-match? (rx bos (* space) "="))))
                       (smart-ops ":="
                                  :action
                                  (lambda (&rest _)
                                    (when (funcall inserting-type? (point))
                                      (just-one-space)
                                      (save-excursion
                                        (insert " ")
                                        (search-backward ":")
                                        (delete-horizontal-space))))))


                     ;; Reformat ':_*' as ': _*'
                     (smart-ops ":_*"
                                :pad-before nil
                                :pad-after nil
                                :action
                                (lambda (&rest _)
                                  (save-excursion
                                    (search-backward "_")
                                    (just-one-space))))
                     (smart-ops ":=>"
                                :pad-before nil
                                :action
                                (lambda (&rest _)
                                  (save-excursion
                                    (search-backward "=")
                                    (just-one-space))))
                     (smart-ops "_=>"
                                :action
                                (lambda (&rest _)
                                  (save-excursion
                                    (search-backward "=")
                                    (just-one-space))))


                     ;; Prevent above smart ops from breaking underscores in
                     ;; symbols.
                     (smart-ops "_" :bypass? t)
                     (smart-ops "__" :bypass? t)
                     (smart-ops "___" :bypass? t)

                     ;; Reformat '=???' as '= ???'
                     (smart-ops "=???" "=???,"
                                :action
                                (lambda (&rest _)
                                  (save-excursion
                                    (skip-chars-backward ",? ")
                                    (just-one-space)))))))

      (define-smart-ops-for-mode 'scala-mode
        (smart-op "///" :action 'scala/replace-slashes-with-doc)
        cb-scala/common-ops)

      (define-smart-ops-for-mode 'sbt-file-mode
        (smart-op "///" :action 'scala/replace-slashes-with-doc)
        cb-scala/common-ops)

      (define-smart-ops-for-mode 'ensime-inf-mode
        (smart-op ":"
                  :pad-before nil
                  :pad-after-unless
                  (lambda (_)
                    (forward-char -1)
                    (cb-scala/at-repl-prompt?)))
        cb-scala/common-ops))))

(defun cb-scala/post-init-popwin ()
  (use-package popwin
    :config
    (progn
      (push '("^\\*sbt\\*" :regexp t :dedicated t :position bottom :stick t :noselect nil :height 33)
            popwin:special-display-config))))

(defun cb-scala/init-scala-errors ()
  (use-package scala-errors))

(defun cb-scala/init-scala-pretty-sbt ()
  (use-package scala-pretty-sbt
    :after sbt-mode))

(defun cb-scala/init-scala-yasnippet ()
  (use-package scala-yasnippet
    :after scala-mode2
    :config
    (cb-yas/register-snippets-dir (f-join scala-yasnippet--root "snippets"))))

(defun cb-scala/init-ensime-diminished-modeline ()
  (use-package ensime-diminished-modeline
    :after ensime))

(defun cb-scala/post-init-indent-dwim ()
  (use-package indent-dwim
    :config
    (add-to-list 'indent-dwim-commands-alist '(scala-mode . ensime-format-source))))

(defun cb-scala/init-sbt-file-mode ()
  (use-package sbt-file-mode
    :mode ("\\.sbt\\'" . sbt-file-mode)))

(defun cb-scala/post-init-flycheck ()
  (use-package flycheck
    :config
    (setq flycheck-scalastylerc "~/.scalastyle.xml")))

(defun cb-scala/init-ensime-flycheck-integration ()
  (use-package ensime-flycheck-integration
    :after (ensime flycheck)
    :config (ensime-flycheck-integration-init)))

(defun cb-scala/init-scala-autoinsert ()
  (use-package scala-autoinsert
    :functions (scala-autoinsert-init)
    :config (scala-autoinsert-init)))

;;; packages.el ends here
