;;; funcs.el --- Helper functions for cb-ledger layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(autoload 'evil-insert-state "evil-states")
(autoload 'ledger-post-align-postings "ledger-post")
(autoload 'ledger-sort-buffer "ledger-sort")
(autoload 'org-read-date "org")

(defun ledger/goto-ledger-file ()
  "Go to the ledger file."
  (interactive)
  (find-file ledger-master-file))

(defun ledger/format-buffer ()
  "Reformat the buffer."
  (interactive "*")
  (let ((pt (point)))
    (save-excursion
      (ledger-post-align-postings (point-min) (point-max))
      (ledger-sort-buffer)
      (message "Formatted buffer"))
    (goto-char pt)))

(defun ledger/insert-timestamp (date)
  "Insert a timestamp at point."
  (interactive (list (org-read-date)))
  (insert (s-replace "-" "/" date))
  (just-one-space)
  (evil-insert-state))

(provide 'funcs)

;;; funcs.el ends here
