;;; extensions.el --- cb-js Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-js-pre-extensions
  '(js))

(defconst cb-js-post-extensions
  '(super-smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-js/init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))

(defun cb-js/init-super-smart-ops ()

  (defun js/smart-colon ()
    (interactive)
    (core/insert-smart-op-no-leading-space ":"))

  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'js-mode
      :custom
      '((":" . js/smart-colon)))))
