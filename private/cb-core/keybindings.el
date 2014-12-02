(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x a a") 'align-regexp)

(bind-key (kbd "C-SPC") 'helm-mini)
(bind-key (kbd "C-<backspace>") 'core/kill-this-buffer)
(bind-key (kbd "C-c k b") 'core/clean-buffers)
(bind-key* "S-SPC" 'smex)

(when (fboundp 'cycle-spacing)
  (global-set-key (kbd "M-SPC") 'cycle-spacing))

(evil-global-set-key 'normal (kbd "C-<up>") 'core/move-line-up)
(evil-global-set-key 'normal (kbd "C-<down>") 'core/move-line-down)
(evil-global-set-key 'normal (kbd "TAB") 'indent-line)

;;; Indentation

(define-key prog-mode-map (kbd "M-q") 'core/indent-dwim)
(evil-define-key 'normal  prog-mode-map (kbd "M-q") 'core/indent-dwim)

(global-set-key (kbd "<backtab>") 'core/outdent)
(evil-global-set-key 'normal (kbd "<backtab>") 'core/outdent)

;;; Exiting Emacs

(bind-key (kbd "C-c k k") 'core/exit-emacs)
(bind-key (kbd "C-x C-c") 'core/warn-exit-emacs-rebound)

;;; Company

(require 'company)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "C-w") nil)

;;; Insertion

(evil-leader/set-key "iF" 'insert-file)
(evil-leader/set-key "iL" 'core/insert-lorem-ipsum)
(evil-leader/set-key "iT" 'core/insert-timestamp)
(evil-leader/set-key "iu" 'insert-char)
(evil-leader/set-key "iU" 'core/insert-uuid)
(evil-leader/set-key "iV" 'add-file-local-variable)
(evil-leader/set-key "iP" 'add-file-local-variable-prop-line)
(evil-leader/set-key "i#" 'core/insert-shebang)
