(evil-leader/set-key "gP" 'magit-push-popup)
(evil-leader/set-key "gc" 'magit-commit-popup)
(evil-leader/set-key "gl" 'magit-log-popup)
(evil-leader/set-key "gr" 'magit-reflog)
(evil-leader/set-key "gD" 'magit-diff-working-tree)
(evil-leader/set-key "gB" 'magit-blame)
(evil-leader/set-key "gb" 'magit-branch-popup)
(evil-leader/set-key "gn" 'diff-hl-next-hunk)
(evil-leader/set-key "gp" 'diff-hl-previous-hunk)

(evil-set-initial-state 'git-commit-mode 'insert)

(evil-global-set-key 'normal (kbd "g b b") 'magit-blame)
(evil-global-set-key 'normal (kbd "g b q") 'magit-blame-quit)
(evil-global-set-key 'normal (kbd "g b n") 'magit-blame-next-chunk)
(evil-global-set-key 'normal (kbd "g b N") 'magit-blame-previous-chunk)
(evil-global-set-key 'normal (kbd "g b p") 'magit-blame-previous-chunk)

(evil-global-set-key 'normal (kbd "g t") 'spacemacs/time-machine-micro-state)

(evil-global-set-key 'normal (kbd "g c") 'magit-commit-popup)
(evil-global-set-key 'normal (kbd "g l") 'magit-log-popup)
(evil-global-set-key 'normal (kbd "g a") 'git/add-this-file)
(evil-global-set-key 'normal (kbd "g n") 'diff-hl-next-hunk)
(evil-global-set-key 'normal (kbd "g p") 'diff-hl-previous-hunk)
(evil-global-set-key 'normal (kbd "g P") 'magit-push-popup)

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-k") 'magit-copy-as-kill)
  (define-key magit-mode-map (kbd "C-y") 'magit-copy-as-kill)
  )
