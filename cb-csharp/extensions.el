;;; extensions.el --- cb-csharp Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-csharp-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defconst cb-csharp-post-extensions
  '(smart-ops)
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-csharp/init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'csharp-mode
      (smart-ops "," ";" :pad-before nil :pad-after t)
      (smart-ops-default-ops))))
