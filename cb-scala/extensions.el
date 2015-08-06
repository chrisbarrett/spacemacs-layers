;;; extensions.el --- cb-scala Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-scala-pre-extensions
  '(
    smart-ops
    scala-errors
    scala-pretty-sbt
    scala-yasnippet
    )
  "List of all extensions to load before the packages.")

(defconst cb-scala-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-scala/init-smart-ops ()
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

      (define-smart-ops-for-mode 'scala-mode
        (smart-ops "???" "?" "=" "==" "+" "-" "@" "*" "/" "<" ">" "|" "$" "&" "%" "!" "~")
        (smart-ops ":" "," :pad-before nil)
        (smart-op "///" :action 'scala/replace-slashes-with-doc)

        ;; Reformat '=???' as '= ???'
        (smart-op "=???"
                  :action
                  (lambda (&rest _)
                    (save-excursion
                      (skip-chars-backward "? ")
                      (just-one-space)))))

      (defun cb-scala/at-repl-prompt? ()
        (s-matches? (rx bol (* space) "scala>" (* space))
                    (buffer-substring (line-beginning-position) (point))))

      (define-smart-ops-for-mode 'ensime-inf-mode
        (smart-ops "???" "?" "=" "==" "+" "-" "@" "*" "/" "<" ">" "|" "$" "&" "%" "!" "~")
        (smart-op "," :pad-before nil)
        (smart-op ":"
                  :pad-before nil
                  :pad-after-unless
                  (lambda (_)
                    (forward-char -1)
                    (cb-scala/at-repl-prompt?)))))))

(defun cb-scala/init-scala-errors ()
  (use-package scala-errors))

(defun cb-scala/init-scala-pretty-sbt ()
  (with-eval-after-load 'sbt-mode
    (require 'scala-pretty-sbt)))

(defun cb-scala/init-scala-yasnippet ()
  (with-eval-after-load 'scala-mode2
    (require 'scala-yasnippet)
    (scala-yasnippet-initialise)))
