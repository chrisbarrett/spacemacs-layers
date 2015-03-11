;;; OS X compat
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)



(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x a a") 'align-regexp)

(global-set-key (kbd "<f5>") 'compile)

(bind-key (kbd "C-<backspace>") 'core/kill-this-buffer)
(bind-key (kbd "C-c k b") 'core/clean-buffers)

(when (fboundp 'cycle-spacing)
  (global-set-key (kbd "M-SPC") 'cycle-spacing))

(evil-global-set-key 'normal (kbd "C-<up>") 'core/move-line-up)
(evil-global-set-key 'normal (kbd "C-<down>") 'core/move-line-down)
(evil-global-set-key 'normal (kbd "TAB") 'indent-line)

(evil-leader/set-key "wo" 'delete-other-windows)
(evil-leader/set-key "|" 'core/toggle-window-split)

;;; Indentation

(define-key prog-mode-map (kbd "RET") 'comment-indent-new-line)
(define-key prog-mode-map (kbd "M-q") 'core/indent-dwim)
(evil-define-key 'normal  prog-mode-map (kbd "M-q") 'core/indent-dwim)

(global-set-key (kbd "<backtab>") 'core/outdent)
(evil-global-set-key 'normal (kbd "<backtab>") 'core/outdent)

;;; Exiting Emacs

(bind-key (kbd "C-c k k") 'core/exit-emacs)
(bind-key (kbd "C-x C-c") 'core/warn-exit-emacs-rebound)

;;; Company

(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-w") nil))

;;; Errors

(evil-global-set-key 'normal (kbd "M-N") 'spacemacs/next-error)
(evil-global-set-key 'normal (kbd "M-P") 'spacemacs/previous-error)
(evil-global-set-key 'insert (kbd "M-N") 'spacemacs/next-error)
(evil-global-set-key 'insert (kbd "M-P") 'spacemacs/previous-error)

;;; Insertion

(evil-leader/set-key "iF" 'insert-file)
(evil-leader/set-key "iL" 'core/insert-lorem-ipsum)
(evil-leader/set-key "iT" 'core/insert-timestamp)
(evil-leader/set-key "iu" 'insert-char)
(evil-leader/set-key "iU" 'core/insert-uuid)
(evil-leader/set-key "iV" 'add-file-local-variable)
(evil-leader/set-key "iP" 'add-file-local-variable-prop-line)
(evil-leader/set-key "i#" 'core/insert-shebang)

(evil-leader/set-key "Fo" 'other-frame)

(evil-set-initial-state 'comint-mode 'normal)

;;; Helm

(bind-key (kbd "C-SPC") 'helm-for-files)

(bind-key* "S-SPC" 'helm-M-x)
(bind-key* "M-x" 'helm-M-x)
(bind-key* "s-b" 'helm-buffers-list)

(after 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-map (kbd "C-SPC")  'helm-toggle-visible-mark)
  )

;;; Occur

(spacemacs|evilify 'occur-mode)
(evil-leader/set-key "oo" 'helm-occur)
(evil-leader/set-key "om" 'helm-multi-occur)
(evil-leader/set-key "O" 'occur)

(evil-ex-define-cmd "nospell"
                    (lambda ()
                      (interactive)
                      (turn-off-flyspell)))

(evil-ex-define-cmd "spell"
                    (lambda ()
                      (interactive)
                      (turn-on-flyspell)))

;;; Frame registers.

(core/make-fn-key-frame-register-command 'f7)
(core/make-fn-key-frame-register-command 'f8)
(core/make-fn-key-frame-register-command 'f9)
