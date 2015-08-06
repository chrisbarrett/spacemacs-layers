;;; extensions.el --- cb-elisp Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-elisp-pre-extensions '(smart-ops))

(defconst cb-elisp-post-extensions '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-elisp/init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'emacs-lisp-mode
      (smart-ops "." ",@" "," :pad-before t :pad-after nil))))

;;; extensions.el ends here
