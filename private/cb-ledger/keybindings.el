(require 'ledger-mode)

(define-key ledger-mode-map (kbd "C-c C-c") 'ledger-report)
(define-key ledger-mode-map (kbd "M-RET") 'ledger-toggle-current-transaction)
(evil-define-key 'normal ledger-report-mode-map (kbd "q") 'kill-buffer-and-window)

(evil-global-set-key 'normal (kbd "SPC o $") 'ledger/goto-ledger-file)
