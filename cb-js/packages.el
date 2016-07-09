;;; packages.el --- cb-js Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'smartparens nil t)
  (require 'use-package nil t))

(defconst cb-js-packages
  '(js2-mode
    js
    smartparens
    smart-ops))

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
      (setq js2-mode-show-strict-warnings nil))))

(defun cb-js/post-init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))

(defun cb-js/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (sp-with-modes '(js-mode js2-mode)
      (sp-local-pair "<" ">")
      (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding)))))

(defun cb-js/post-init-smart-ops ()
  (use-package smart-ops
    :defer t
    :config
    (let ((default-ops (-mapcat #'smart-op (-difference smart-ops-default-ops '("<" ">" "!")))))

      (defun cb-js/inside-tags? ()
        (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
          (equal op "<")))

      (define-smart-ops-for-mode 'js-mode
        (smart-ops ";" ":" "," :pad-before nil)
        default-ops)

      (define-smart-ops-for-mode 'js2-mode
        (smart-ops ";" ":" "," :pad-before nil)
        default-ops))))
