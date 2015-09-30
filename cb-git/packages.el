;;; packages.el --- cb-git Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-git-packages
  '(magit
    git-auto-commit-mode))

(defun cb-git/post-init-magit ()
  ;; Remove broken Spacemacs customisation
  (remove-hook 'git-commit-mode-hook 'fci-mode)

  (core/remap-face 'magit-section-highlight 'core/bg-hl-ok)
  (core/remap-face 'magit-diff-context-highlight 'core/bg-hl-ok))

(defun cb-git/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :diminish git-auto-commit-mode
    :init
    (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))))
