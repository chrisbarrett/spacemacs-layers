;;; extensions.el --- cb-rust Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-rust-pre-extensions
  '(super-smart-ops)
  "List of all extensions to load before the packages.")

(defconst cb-rust-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-rust/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'rust-mode
      :rem '("!" "~" "&")
      :custom
      `((":" . rust/smart-colon)
        ("," . ,(super-smart-ops-make-smart-op "," nil t))))))
