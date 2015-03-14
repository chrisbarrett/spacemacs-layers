;;; extensions.el --- cb-sql Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar cb-sql-pre-extensions
  '(
    ;; pre extension cb-sqls go here
    sql
    )
  "List of all extensions to load before the packages.")

(defvar cb-sql-post-extensions
  '(
    ;; post extension cb-sqls go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-sql/init-<extension-cb-sql>
;;
;; (defun cb-sql/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-sql/init-sql ()
  (use-package sql
    :defer t
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'sql-interactive-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)

      (define-key sql-mode-map (kbd "C-c C-z") 'sql-product-interactive))))
