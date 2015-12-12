;;; packages.el --- cb-scala Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'dash nil t)
  (require 's nil t))

(defconst cb-scala-packages
  '(scala-mode2
    sbt-mode
    ensime
    smart-ops

    (scala-errors :location local)
    (scala-pretty-sbt :location local)
    (scala-yasnippet :location local)
    (ensime-diminished-modeline :location local)
    ))

(defun cb-scala/post-init-scala-mode2 ()
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

      (add-hook 'scala-mode-hook 'scala/set-normal-state-local-keybindings))))

(defun cb-scala/post-init-ensime ()
  (add-hook 'scala-mode-hook 'scala/maybe-start-ensime)
  (add-hook 'ensime-mode-hook 'cb-core/turn-off-aggressive-indent-mode)

  (setq ensime-auto-generate-config t)
  (setq ensime-prefer-noninteractive t)
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


      ;;; Fix up ensime files before loading
  (defadvice ensime-config-load (before fix-ensime-file activate)
    (scala/fix-ensime-file))

  (with-eval-after-load 'ensime-inf
    (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src))

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

  (evil-leader/set-key-for-mode 'scala-mode "ii" 'ensime-import-type-at-point))

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
    (cb-core/turn-off-aggressive-indent-mode)
    (show-smartparens-mode -1)
    (show-paren-mode -1)
    (local-set-key (kbd "C-l") 'spacemacs/comint-clear-buffer)
    (local-set-key (kbd "C-c RET") 'scala/sbt-send-ret))

  (add-hook 'sbt-mode-hook 'cb-scala/set-up-sbt-mode))

(defun cb-scala/post-init-smart-ops ()
  (use-package smart-ops
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
                     (smart-op "=???"
                               :action
                               (lambda (&rest _)
                                 (save-excursion
                                   (skip-chars-backward "? ")
                                   (just-one-space)))))))

      (define-smart-ops-for-mode 'scala-mode
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

(defun cb-scala/init-scala-errors ()
  (use-package scala-errors))

(defun cb-scala/init-scala-pretty-sbt ()
  (with-eval-after-load 'sbt-mode
    (require 'scala-pretty-sbt)))

(defun cb-scala/init-scala-yasnippet ()
  (with-eval-after-load 'scala-mode2
    (require 'scala-yasnippet)
    (cb-yas/register-snippets-dir (f-join scala-yasnippet--root "snippets"))))

(defun cb-scala/init-ensime-diminished-modeline ()
  (use-package ensime-diminished-modeline))
