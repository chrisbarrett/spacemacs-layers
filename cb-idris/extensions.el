;;; extensions.el --- cb-idris Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-idris-pre-extensions
  '(super-smart-ops)
  "List of all extensions to load before the packages.")

(defconst cb-idris-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-idris/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'idris-mode
        :add '("$")
        :custom
        '(("?" . idris/smart-question-mark)
          ("|" . idris/smart-pipe)
          ("." . idris/smart-dot)
          ("," . idris/smart-comma)
          (":" . idris/smart-colon)))

      (super-smart-ops-configure-for-mode 'idris-repl-mode
        :add '("$")
        :custom
        '(("?" . idris/smart-question-mark)
          ("|" . idris/smart-pipe)
          ("." . idris/smart-dot)
          ("," . idris/smart-comma)
          (":" . idris/smart-colon))))))
