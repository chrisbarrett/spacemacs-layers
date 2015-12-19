;;; packages.el --- cb-mu4e Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'dash nil t)
  (require 'f nil t)
  (require 'spaceline nil t))

(defconst cb-mu4e-packages
  '(async
    (mu4e :location local)
    (mu4e-multi :location local)
    (mu4e-unread-messages :location local)))

(defun cb-mu4e/init-async ()
  ;; Send mail asynchronously.
  (use-package async
    :init
    (setq sendmail-program "msmtp")
    :config
    (progn
      (require 'smtpmail-async)

      ;; Tweaks to smtpmail-async so that I can inject msmtp as the sendmail
      ;; program.
      (defun cb-async-smtpmail-send-it ()
        (let ((to          (message-field-value "To"))
              (buf-content (buffer-substring-no-properties (point-min) (point-max))))
          (message "Delivering message to %s..." to)
          (async-start
           `(lambda ()
              (require 'smtpmail)
              (with-temp-buffer
                (insert ,buf-content)
                (set-buffer-multibyte nil)
                ;; Pass in the variable environment for smtpmail
                ,(async-inject-variables
                  "\\`\\(smtpmail\\|sendmail\\|async-smtpmail\\|\\(user-\\)?mail\\)-\\|auth-sources"
                  nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
                (run-hooks 'async-smtpmail-before-send-hook)
                (smtpmail-send-it)))
           `(lambda (&optional ignore)
              (message "Delivering message to %s...done" ,to)))))

      (setq send-mail-function 'cb-async-smtpmail-send-it)
      (setq message-send-mail-function 'cb-async-smtpmail-send-it))))

(defun cb-mu4e/init-mu4e ()
  (use-package mu4e
    :defer 10
    :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
    :init
    (evil-leader/set-key "am" 'mu4e)
    :config
    (progn
      (with-eval-after-load 'org
        (require 'org-mu4e))

      ;; Enable evil leader in mu4e buffers.
      (add-to-list 'evil-leader/no-prefix-mode-rx "mu4e-main-mode")
      (add-to-list 'evil-leader/no-prefix-mode-rx "mu4e-headers-mode")
      (add-to-list 'evil-leader/no-prefix-mode-rx "mu4e-view-mode")
      (define-key mu4e-main-mode-map (kbd "SPC") nil)
      (define-key mu4e-view-mode-map (kbd "SPC") nil)
      (define-key mu4e-headers-mode-map (kbd "SPC") nil)

      ;; Vim-style navigation.

      (define-key mu4e-headers-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir)
      (define-key mu4e-headers-mode-map (kbd "j") 'mu4e-headers-next)
      (define-key mu4e-headers-mode-map (kbd "k") 'mu4e-headers-prev)

      (define-key mu4e-view-mode-map (kbd "J") 'mu4e~view-headers-jump-to-maildir)
      (define-key mu4e-view-mode-map (kbd "j") 'mu4e-view-headers-next)
      (define-key mu4e-view-mode-map (kbd "k") 'mu4e-view-headers-prev)

      ;; Bury mu4e rather than killing it.
      (define-key mu4e-main-mode-map (kbd "q") 'bury-buffer)

      ;; Set variables

      (setq mu4e-confirm-quit nil)
      (setq mu4e-use-fancy-chars t)
      (setq mu4e-headers-attach-mark (purecopy '("a" . "📎")))
      (setq mu4e-headers-unread-mark (purecopy '("u" . "●")))
      (setq mu4e-hide-index-messages t)

      (setq mu4e-view-show-images t)
      (setq message-kill-buffer-on-exit t)

      ;; Use offlineimap to manage maildir.
      (setq mu4e-get-mail-command "offlineimap")

      ;; Save attachments to Downloads dir.
      (setq mu4e-attachment-dir (f-expand "~/Downloads"))

      ;; Disable signatures.
      (setq mu4e-compose-signature nil)
      (setq mu4e-compose-signature-auto-include nil)

      ;; Put quoted messages after signature.
      (setq message-forward-before-signature nil)

      ;; Use standard citation style.
      (setq message-citation-line-function 'message-insert-formatted-citation-line)
      (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

      ;; Update every minute.
      (setq mu4e-update-interval 60)

      ;; use imagemagick, if available
      (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))

      ;; Render html emails using something reasonable.
      (setq mu4e-html2text-command
            (cond
             ((executable-find "textutil")
              "textutil -stdin -format html -convert txt -stdout")
             ((executable-find "w3m")
              "w3m -dump -cols 80 -T text/html")
             (t
              'html2text)))

      ;; View html message in eww. `av` in view to activate
      (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

      ;; View html message in external browser. `a&` in view to activate

      (defun cb-mu4e-action-view-in-external-browser (msg)
        (let ((browse-url-browser-function 'browse-url-default-browser))
          (mu4e-action-view-in-browser msg)))

      (add-to-list 'mu4e-view-actions '("&viewInExternalBrowser" . cb-mu4e-action-view-in-external-browser) t)

      (setq browse-url-mailto-function 'mu4e-multi-compose-new)

      ;; Start at the 'To' header when composing.
      (defun cb-mu4e-goto-to-header ()
        (interactive)
        ;; HACK: I can't figure out how to hook into this mode correctly and
        ;; that makes me sad. :(
        (run-with-timer 0.03 nil
                        (lambda ()
                          (goto-char (point-min))
                          (search-forward "to:" nil t)
                          (evil-append-line 1)
                          (just-one-space))))

      (add-hook 'message-mode-hook 'cb-mu4e-goto-to-header)



      ;; Add format=flowed so receiving clients can format plain text correctly.
      (defun cb-mu4e-flow-text ()
        (use-hard-newlines t 'guess))

      (add-hook 'mu4e-compose-mode-hook 'cb-mu4e-flow-text)

      ;; Update in background soon after starting.
      (run-with-timer 5 nil 'mu4e-update-mail-and-index t))))

(defun cb-mu4e/init-mu4e-multi ()
  (use-package mu4e-multi
    :config
    (progn
      (global-set-key (kbd "C-x m") 'mu4e-multi-compose-new)
      ;; See `mu4e-multi-account-alist' in personal-config file for accounts
      ;; configuration (not on GitHub).
      (with-eval-after-load 'personal-config (mu4e-multi-enable)))))

(defun cb-mu4e/init-mu4e-unread-messages ()
  (use-package mu4e-unread-messages
    :config
    (progn
      (setq display-time-use-mail-icon t)
      (setq display-time-mail-function 'mu4e-unread-messages?)
      (display-time-mode +1))))
