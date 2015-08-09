;;; extensions.el --- cb-idris Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-idris-pre-extensions
  '(smart-ops)
  "List of all extensions to load before the packages.")

(defconst cb-idris-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

(defun cb-idris/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (defun idris/looking-at-module-or-constructor? (&rest _)
        (-when-let ([fst] (thing-at-point 'symbol))
          (s-uppercase? fst)))

      (define-smart-ops-for-mode 'idris-mode
        (smart-ops "?" "$" "|" ":")
        (smart-ops "." :pad-unless 'idris/looking-at-module-or-constructor?)
        (smart-ops-default-ops))

      (define-smart-ops-for-mode 'idris-repl-mode
        (smart-ops "?" "$" "|" ":")
        (smart-ops "." :pad-unless 'idris/looking-at-module-or-constructor?)
        (smart-ops-default-ops)))))
