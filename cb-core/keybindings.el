;;; OS X compat
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-n") 'new-frame)
(global-set-key (kbd "s-w") 'delete-frame)



(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x a a") 'align-regexp)

(when (fboundp 'cycle-spacing)
  (global-set-key (kbd "M-SPC") 'cycle-spacing))

(evil-global-set-key 'normal (kbd "C-<up>") 'cb-core-move-line-up)
(evil-global-set-key 'normal (kbd "C-<down>") 'cb-core-move-line-down)

;;; Exiting Emacs

(bind-key (kbd "C-c k k") 'cb-core-exit-emacs)
(bind-key (kbd "C-x C-c") 'cb-core-warn-exit-emacs-rebound)

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

;;; Helm

(bind-key (kbd "C-SPC") 'helm-for-files)

(bind-key* "S-SPC" 'helm-M-x)
(bind-key* "M-x" 'helm-M-x)
(bind-key* "s-b" 'helm-buffers-list)

(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-map (kbd "C-SPC")  'helm-toggle-visible-mark)
  )

;;; Occur

(with-eval-after-load 'occur
  (evilified-state-evilify occur-mode occur-mode-map))

(spacemacs/set-leader-keys "oo" 'helm-occur)
(spacemacs/set-leader-keys "om" 'helm-multi-occur)
(spacemacs/set-leader-keys "O" 'occur)

(spacemacs/set-leader-keys (kbd "ww") 'ace-window)
