(add-hook 'term-mode (lambda () (hl-line-mode -1)))
(setq explicit-shell-file-name (-first 'f-exists? '("/usr/local/bin/zsh" "/usr/bin/zsh")))
