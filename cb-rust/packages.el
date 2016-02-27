;;; packages.el --- cb-rust Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-rust-packages
  '(smart-ops))

(defun cb-rust/post-init-smart-ops ()
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops ";" "," "~" "&" ":" :pad-before nil)

    (smart-ops "." "::"
               :pad-before nil :pad-after nil
               :action #'company-manual-begin)

    ;; Position point inside template braces.
    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _) (search-backward ">")))

    (smart-ops-default-ops)))

(provide 'packages)

;;; packages.el ends here
