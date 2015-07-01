;;; packages.el --- wanderlust Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-wanderlust-packages '(wanderlust))

(defconst cb-wanderlust-excluded-packages '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-wanderlust/init-wanderlust ()
  (use-package wl
    :init
    (progn
      ;; Use wanderlust to compose emails

      (autoload 'wl-user-agent-compose "wl-draft" nil t)

      (setq mail-user-agent 'wl-user-agent)

      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

    :config
    (progn

      (add-hook 'wl-folder-mode-hook (lambda () (evil-emacs-state +1)) t)
      (add-hook 'wl-summary-mode-hook (lambda () (evil-emacs-state +1)) t)

      (setq wl-icon-directory nil)
      (setq wl-demo nil)
      (setq wl-folder-check-async t)
      (setq wl-summary-showto-folder-regexp ".*Sent.*")

      (define-key wl-folder-mode-map (kbd "j") 'wl-folder-next-entity)
      (define-key wl-folder-mode-map (kbd "k") 'wl-folder-prev-entity)

      (define-key wl-summary-mode-map (kbd "j") 'wl-summary-next)
      (define-key wl-summary-mode-map (kbd "k") 'wl-summary-prev)

      ;;; HACK: WL's message buffer mode is not a real major mode.

      (defadvice wl-message-mode (after configure-mode activate)
        (evil-local-set-key 'normal "q" 'mime-preview-quit))

      (defadvice wl-setup-summary (after emacs-state activate)
        (evil-emacs-state +1))

      ;;; Hide boring message headers

      ;; Ignore all fields...
      (setq wl-message-ignored-field-list '("^.*:"))
      (setq mime-view-ignored-field-list wl-message-ignored-field-list)
      ;; ..but these five
      (setq wl-message-visible-field-list
            '("^To:"
              "^Cc:"
              "^From:"
              "^Subject:"
              "^Date:"))
      (setq mime-view-visible-field-list wl-message-visible-field-list))))
