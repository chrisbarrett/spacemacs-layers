(add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))

;;; Configure git commit mode

(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(put 'git-commit-mode 'fill-column 72)
