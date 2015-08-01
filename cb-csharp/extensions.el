;;; extensions.el --- cb-csharp Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-csharp-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defconst cb-csharp-post-extensions
  '(super-smart-ops)
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-csharp/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'csharp-mode
      :add '("?")
      :custom
      `(("," . ,(super-smart-ops-make-smart-op "," nil t))
        (";" . ,(super-smart-ops-make-smart-op ";" nil t))))))
