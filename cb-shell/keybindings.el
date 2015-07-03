(global-set-key (kbd "<f1>") 'cb-shell-term-bring)

(evil-set-initial-state 'term-mode 'emacs)

(defadvice term-char-mode (after set-keybindings activate)
  (local-set-key (kbd "C-c RET") 'term-line-mode))

(defadvice term-line-mode (after set-keybindings activate)
  (local-set-key (kbd "C-c RET") 'term-char-mode))
