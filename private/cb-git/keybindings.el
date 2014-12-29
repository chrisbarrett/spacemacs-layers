(evil-leader/set-key "gP" 'magit-key-mode-popup-pushing)
(evil-leader/set-key "gc" 'magit-key-mode-popup-committing)
(evil-leader/set-key "gl" 'magit-key-mode-popup-logging)
(evil-leader/set-key "gr" 'magit-reflog)
(evil-leader/set-key "gD" 'magit-diff-working-tree)
(evil-leader/set-key "gB" 'magit-blame-mode)
(evil-leader/set-key "gb" 'magit-key-mode-popup-branching)
(evil-leader/set-key "gn" 'git-gutter:next-hunk)
(evil-leader/set-key "gp" 'git-gutter:previous-hunk)

(evil-set-initial-state 'git-commit-mode 'insert)

(evil-global-set-key 'normal (kbd "g c") 'magit-key-mode-popup-committing)
(evil-global-set-key 'normal (kbd "g l") 'magit-key-mode-popup-logging)
(evil-global-set-key 'normal (kbd "g a") 'git/add-this-file)
(evil-global-set-key 'normal (kbd "g s") 'git-gutter:stage-hunk)
(evil-global-set-key 'normal (kbd "g r") 'git-gutter:revert-hunk)
(evil-global-set-key 'normal (kbd "g n") 'git-gutter:next-hunk)
(evil-global-set-key 'normal (kbd "g p") 'git-gutter:previous-hunk)
(evil-global-set-key 'normal (kbd "g P") 'magit-key-mode-popup-pushing)

(after 'magit
  (define-key magit-mode-map (kbd "C-k") 'magit-copy-item-as-kill)
  )
