(evil-global-set-key 'normal (kbd "SPC g P") 'magit-key-mode-popup-pushing)
(evil-global-set-key 'normal (kbd "SPC g c") 'magit-key-mode-popup-committing)
(evil-global-set-key 'normal (kbd "SPC g l") 'magit-key-mode-popup-logging)
(evil-global-set-key 'normal (kbd "SPC g r") 'magit-reflog)
(evil-global-set-key 'normal (kbd "SPC g D") 'magit-diff-working-tree)
(evil-global-set-key 'normal (kbd "SPC g B") 'magit-blame-mode)
(evil-global-set-key 'normal (kbd "SPC g b") 'magit-key-mode-popup-branching)
(evil-global-set-key 'normal (kbd "SPC g n") 'git-gutter:next-hunk)
(evil-global-set-key 'normal (kbd "SPC g p") 'git-gutter:previous-hunk)
(evil-global-set-key 'normal (kbd "SPC g h") 'git-gutter:popup-hunk)
(evil-global-set-key 'normal (kbd "SPC g x") 'git-gutter:revert-hunk)
(evil-global-set-key 'normal (kbd "SPC g a") 'git-gutter:stage-hunk)

(evil-set-initial-state 'git-commit-mode 'insert)
