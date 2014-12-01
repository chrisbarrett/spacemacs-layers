(after 'idris-mode
  (define-key idris-mode-map (kbd "RET") 'idris/ret)
  (define-key idris-mode-map (kbd "M-RET") 'idris/meta-ret)
  (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)
  )

(after 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris/switch-to-src))
