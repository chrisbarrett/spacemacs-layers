(add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))

;;; Configure git commit mode

(evil-set-initial-state 'git-commit-mode 'insert)
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(put 'git-commit-mode 'fill-column 72)
