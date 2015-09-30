;;; packages.el --- cb-rust Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-rust-packages
  '(smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-rust/post-init-smart-ops ()
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops ";" ":" "," :pad-before nil)
    (smart-ops-default-ops)))
