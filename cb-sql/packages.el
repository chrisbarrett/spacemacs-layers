;;; extensions.el --- cb-sql Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-sql-packages
  '(sql
    aggressive-indent
    (cb-sql-find-attrs-with-type :location local)))

(defun cb-sql/init-sql ()
  (use-package sql
    :bind
    (:map sql-mode-map ("C-c C-z" . sql-product-interactive))
    :config
    (progn
      (setq sql-product 'postgres)
      (add-hook 'sql-mode-hook 'sql-highlight-product))))

(defun cb-sql/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'sql-interactive-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)))

(defun cb-sql/init-cb-sql-find-attrs-with-type ()
  (use-package cb-sql-find-attrs-with-type
    :commands cb-sql-find-attrs-with-type
    :evil-bind
    (:state normal :map cb-sql-find-attrs-with-type-mode-map
            ("q" . quit-window)
            ([down-mouse-1] . push-button))

    :config
    (with-eval-after-load 'aggressive-indent
      (add-to-list 'aggressive-indent-excluded-modes 'cb-sql-find-attrs-with-type-mode))))

;;; packages.el ends here
