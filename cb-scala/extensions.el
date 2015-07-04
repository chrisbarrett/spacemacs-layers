;;; extensions.el --- cb-scala Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-scala-pre-extensions
  '(
    super-smart-ops
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

(defun cb-scala/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'scala-mode
        :add '("?")
        :custom
        '(("=" . scala/equals)
          (":" . scala/colon)
          ("+" . scala/plus)
          ("-" . scala/minus)
          ("/" . scala/slash)
          ("," . core/comma-then-space)))

      (super-smart-ops-configure-for-mode 'ensime-inf-mode
        :add '("?")
        :custom
        '(("=" . scala/equals)
          (":" . scala/repl-colon)
          ("+" . scala/plus)
          ("-" . scala/minus)
          ("," . core/comma-then-space))))))

(defun cb-scala/init-scala-errors ()
  (use-package scala-errors))

(defun cb-scala/init-scala-pretty-sbt ()
  (with-eval-after-load 'sbt-mode
    (require 'scala-pretty-sbt)))

(defun cb-scala/init-scala-yasnippet ()
  (with-eval-after-load 'scala-mode2
    (require 'scala-yasnippet)
    (scala-yasnippet-initialise)))
