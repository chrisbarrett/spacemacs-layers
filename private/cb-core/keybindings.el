(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)

(bind-key (kbd "C-SPC") 'helm-mini)
(bind-key (kbd "S-SPC") 'execute-extended-command)
(bind-key (kbd "C-<backspace>") 'core/kill-this-buffer)
(bind-key (kbd "C-c k b") 'core/clean-buffers)

(when (fboundp 'cycle-spacing)
  (global-set-key (kbd "M-SPC") 'cycle-spacing))

(evil-global-set-key 'normal (kbd "C-<up>") 'core/move-line-up)
(evil-global-set-key 'normal (kbd "C-<down>") 'core/move-line-down)
(evil-global-set-key 'normal (kbd "TAB") 'indent-line)

;;; Exiting Emacs

(bind-key (kbd "C-c k k") 'core/exit-emacs)
(bind-key (kbd "C-x C-c") 'core/warn-exit-emacs-rebound)
