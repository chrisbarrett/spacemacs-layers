;;; funcs.el --- Helper functions for cb-ledger layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(autoload 'evil-insert-state "evil-states")
(autoload 'org-read-date "org")

(defun cb-ledger-goto-ledger-file ()
  "Go to the ledger file."
  (interactive)
  (find-file ledger-master-file))

(defun cb-ledger-insert-timestamp (date)
  "Insert a timestamp at point."
  (interactive (list (org-read-date)))
  (insert (s-replace "-" "/" date))
  (just-one-space)
  (evil-insert-state))

(provide 'funcs)

;;; funcs.el ends here
