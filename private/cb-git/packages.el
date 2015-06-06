;;; packages.el --- cb-git Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-git-packages
  '(
    git-auto-commit-mode
    git-commit-mode
    diff-hl
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-git-excluded-packages
  '(git-gutter git-gutter-fringe)
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-git/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :diminish git-auto-commit-mode
    :init
    (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))))

(defun cb-git/init-git-commit-mode ()
  (use-package git-commit-mode
    :config
    (progn
      (add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
      (put 'git-commit-mode 'fill-column 72)

      (add-hook 'git-commit-mode-hook 'end-of-line)

      (defadvice git-commit-commit (after kill-commit-buffer activate)
        "Ensure the commit buffer is killed."
        (-when-let (buf (get-buffer "COMMIT_EDITMSG"))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(defun cb-git/init-diff-hl ()
  (use-package diff-hlt
    :defer t
    :init
    (progn
      (setq diff-hl-side 'right)
      (add-hook 'prog-mode-hook 'diff-hl-mode)
      (add-hook 'dired-mode-hook 'diff-hl-dired-mode))))
