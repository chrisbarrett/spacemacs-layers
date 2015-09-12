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
      (defun cb-idris/looking-at-module-or-constructor? (&rest _)
        (-when-let ([fst] (thing-at-point 'symbol))
          (s-uppercase? fst)))

      (defconst cb-idris/smart-ops
        (-flatten-n 1
                    (list
                     (smart-ops "?" :pad-after nil)
                     (smart-ops "$" "|" ":")
                     (smart-ops "." :pad-unless 'cb-idris/looking-at-module-or-constructor?)
                     (smart-ops-default-ops))))

      (define-smart-ops-for-mode 'idris-mode cb-idris/smart-ops)
      (define-smart-ops-for-mode 'idris-repl-mode cb-idris/smart-ops))))
