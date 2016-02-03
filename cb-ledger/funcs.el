;;; funcs.el --- Helper functions for cb-ledger layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(autoload 'evil-insert-state "evil-states")
(autoload 'ledger-mode-clean-buffer "ledger-mode")
(autoload 'ledger-post-align-postings "ledger-post")
(autoload 'ledger-sort-buffer "ledger-sort")
(autoload 'org-read-date "org")

(defun cb-ledger-goto-ledger-file ()
  "Go to the ledger file."
  (interactive)
  (find-file ledger-master-file))

(defun cb-ledger-format-buffer ()
  "Reformat the buffer."
  (interactive "*")
  (let ((pos (point)))
    (ledger-mode-clean-buffer)
    (goto-char (point-min))
    (while (search-forward "=" nil t)
      (cb-ledger--align-price-assertion))
    (goto-char pos)))

(defun cb-ledger--align-price-assertion ()
  (when (s-contains? "=" (buffer-substring (line-beginning-position) (line-end-position)))
    (unwind-protect
        (progn
          (goto-char (line-beginning-position))
          (search-forward "=")
          (goto-char (match-beginning 0))
          (indent-to (1+ ledger-post-amount-alignment-column))
          (skip-chars-forward " =")
          (just-one-space))
      (goto-char (line-end-position)))))

(defun cb-ledger-insert-timestamp (date)
  "Insert a timestamp at point."
  (interactive (list (org-read-date)))
  (insert (s-replace "-" "/" date))
  (just-one-space)
  (evil-insert-state))

(provide 'funcs)

;;; funcs.el ends here
