;;; packages.el --- cb-org-reveal Layer packages File for Spacemacs
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-org-reveal-packages '(ox-reveal))

(defun cb-org-reveal/init-ox-reveal ()
  (use-package ox-reveal
    :config
    (let ((base (f-dirname (symbol-file 'cb-org-reveal-packages))))
      (setq org-reveal-root (f-join base "submodules" "reveal.js")))))

;;; End
