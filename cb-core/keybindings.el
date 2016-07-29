;;; keybindings.el --- General keybindings that should be set unconditionally.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; OS X compat
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-n") 'new-frame)
(global-set-key (kbd "s-w") 'delete-frame)

(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x a a") 'align-regexp)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;; Personal config

(defun cb-core/goto-personal-config ()
  (interactive)
  (find-library "personal-config"))

(spacemacs/set-leader-keys "fep" #'cb-core/goto-personal-config)

;;; Errors

(evil-global-set-key 'normal (kbd "M-N") 'spacemacs/next-error)
(evil-global-set-key 'normal (kbd "M-P") 'spacemacs/previous-error)
(evil-global-set-key 'insert (kbd "M-N") 'spacemacs/next-error)
(evil-global-set-key 'insert (kbd "M-P") 'spacemacs/previous-error)

;;; Insertion

(spacemacs/set-leader-keys "iF" 'insert-file)
(spacemacs/set-leader-keys "iu" 'insert-char)
(spacemacs/set-leader-keys "iV" 'add-file-local-variable)
(spacemacs/set-leader-keys "iP" 'add-file-local-variable-prop-line)
(spacemacs/set-leader-keys "Fo" 'other-frame)

;;; keybindings.el ends here
