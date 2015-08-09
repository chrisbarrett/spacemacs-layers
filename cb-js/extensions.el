;;; extensions.el --- cb-js Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-js-pre-extensions
  '(js))

(defconst cb-js-post-extensions
  '(smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-js/init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))

(defun cb-js/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (define-smart-ops-for-mode 'js-mode
        (smart-ops ";" ":" "," :pad-before nil)
        (smart-ops-default-ops))

      (define-smart-ops-for-mode 'js2-mode
        (smart-ops ";" ":" "," :pad-before nil)
        (smart-ops-default-ops)))))
