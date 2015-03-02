(setq term-prompt-regexp  "^[^#$%>❯\n]*[#$%>❯] *")
(setq term-ansi-default-program (-first 'f-exists? '("/usr/local/bin/zsh"
                                                     "/usr/bin/zsh")))
