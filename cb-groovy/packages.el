;;; packages.el --- cb-groovy layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-groovy-packages
  '(groovy-mode
    smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :mode ("\\.groovy$" . groovy-mode)
    :config
    (progn
      (setq groovy-home "/usr/local/"))))

(defun cb-groovy/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (add-hook 'groovy-mode-hook #'smart-ops-mode)

      (define-smart-ops-for-mode 'groovy-mode
        (smart-ops ":" "," :pad-before nil)
        (smart-ops-default-ops))

      (define-smart-ops-for-mode 'inferior-groovy-mode
        (smart-ops ":" "," :pad-before nil)
        (smart-ops-default-ops)))))
