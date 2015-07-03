;;; packages.el --- cb-git Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-git-packages
  '(
    ido-completing-read+
    git-auto-commit-mode
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
