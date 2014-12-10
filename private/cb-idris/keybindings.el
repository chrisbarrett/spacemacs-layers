(evil-set-initial-state 'idris-prover-script-mode 'insert)

(after 'idris-mode
  (define-key idris-mode-map (kbd "RET") 'idris/ret)
  (define-key idris-mode-map (kbd "M-RET") 'idris/meta-ret)
  (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)
  (define-key idris-mode-map (kbd "SPC") 'idris/smart-space)
  (define-key idris-mode-map (kbd "<backspace>")   'haskell/backspace)
  )

(after 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris/switch-to-src))
