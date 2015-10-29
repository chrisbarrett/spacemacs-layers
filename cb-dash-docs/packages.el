;;; extensions.el --- cb-dash-docs Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-dash-docs-packages
  '(dash-at-point))

(defun cb-dash-docs/init-dash-at-point ()
  (use-package dash-at-point
    :bind ("C-h C-d" . dash-at-point)))
