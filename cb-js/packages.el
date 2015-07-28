;;; packages.el --- cb-js Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-js-packages
  '(js2-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-js-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-js/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.json\\'" . js2-mode))
    :init
    (progn
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
      (defalias 'json-mode 'js2-mode))
    :config
    (progn
      (setq js2-basic-offset 2)
      ;; Use flycheck for checking.
      (setq js2-mode-show-parse-errors nil)
      (setq js2-mode-show-strict-warnings nil)

      (define-key js2-mode-map (kbd "RET") 'js/ret)
      (define-key js2-mode-map (kbd "<backspace>") 'js/backspace)
      (define-key js2-mode-map (kbd "SPC") 'js/space))))
