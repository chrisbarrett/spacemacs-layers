;; HACK: Work around spacemacs clobbering this keybinding
(add-hook 'after-init-hook
          (lambda ()
            (evil-leader/set-key "o$" 'ledger/goto-ledger-file)))

(after 'ledger-mode
  (define-key ledger-mode-map (kbd "C-c C-c") 'ledger-report)
  (define-key ledger-mode-map (kbd "M-RET")   'ledger-toggle-current-transaction)
  (define-key ledger-mode-map (kbd "M-q")     'ledger/format-buffer)
  (define-key ledger-mode-map (kbd "C-c C-.") 'ledger/insert-timestamp)

  (evil-define-key 'normal ledger-report-mode-map (kbd "q") 'kill-buffer-and-window))
