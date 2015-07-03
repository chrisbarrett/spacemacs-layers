;;; extensions.el --- cb-sql Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-sql-pre-extensions
  '(sql)
  "List of all extensions to load before the packages.")

(defvar cb-sql-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-sql/init-sql ()
  (use-package sql
    :defer t
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'sql-interactive-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)

      (define-key sql-mode-map (kbd "C-c C-z") 'sql-product-interactive))))
