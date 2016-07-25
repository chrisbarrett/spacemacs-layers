;;; extensions.el --- cb-sql Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-sql-packages
  '(aggressive-indent
    sql))

(defun cb-sql/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'sql-interactive-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)))

(defun cb-sql/init-sql ()
  (use-package sql
    :bind
    (:map sql-mode-map ("C-c C-z" . sql-product-interactive))
    :config
    (progn
      (setq sql-product 'postgres)
      (add-hook 'sql-mode-hook 'sql-highlight-product))))
