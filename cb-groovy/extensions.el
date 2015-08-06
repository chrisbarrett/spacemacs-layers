;;; extensions.el --- cb-groovy Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-groovy-pre-extensions '()
  "List of all extensions to load before the packages.")

(defconst cb-groovy-post-extensions '(smart-ops)
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-groovy/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (define-smart-ops-for-mode 'groovy-mode
        (smart-ops ":" "," :pad-before nil))

      (define-smart-ops-for-mode 'inferior-groovy-mode
        (smart-ops ":" "," :pad-before nil)))))
