;;; extensions.el --- cb-rust Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-rust-pre-extensions
  '(smart-ops)
  "List of all extensions to load before the packages.")

(defconst cb-rust-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-rust/post-init-smart-ops ()
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops "~" "&" ":" :pad-before nil)

    (smart-ops "." "::"
               :pad-before nil :pad-after nil
               :action 'company-manual-begin)

    ;; Position point inside template braces.
    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _) (search-backward ">")))

    (smart-ops-default-ops)))
