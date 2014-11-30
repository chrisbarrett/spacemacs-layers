(require 'ledger-mode)

(define-key ledger-mode-map (kbd "C-c C-c") 'ledger-report)
(define-key ledger-mode-map (kbd "M-RET") 'ledger-toggle-current-transaction)
