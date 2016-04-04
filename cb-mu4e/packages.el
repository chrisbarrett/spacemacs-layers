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
              (condition-case err
                  (with-temp-buffer
                    (insert ,buf-content)
                    (set-buffer-multibyte nil)
                    ;; Pass in the variable environment for smtpmail
                    ,(async-inject-variables
                      "\\`\\(smtpmail\\|sendmail\\|async-smtpmail\\|\\(user-\\)?mail\\)-\\|auth-sources"
                      nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
                    (run-hooks 'async-smtpmail-before-send-hook)
                    (smtpmail-send-it)
                    (cons t nil))
                (error
                 (cons nil err))))
           `(-lambda ((success? . err))
              (if success?
                  (message "Delivering message to %s...done" ,to)
                (message "Delivering message to %s...FAILED. Error: %s" ,to err))))))

      (setq send-mail-function 'cb-async-smtpmail-send-it)
      (setq message-send-mail-function 'cb-async-smtpmail-send-it))))

(defun cb-mu4e/init-mu4e ()
  (use-package mu4e
    :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
    :commands (mu4e mu4e-compose-new)
    :bind (("C-x m" . mu4e-compose-new))
    :init
    (spacemacs/set-leader-keys "am" 'mu4e)
    :config
    (progn
      (with-eval-after-load 'org
        (require 'org-mu4e))

      (evilified-state-evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "j") 'mu4e~headers-jump-to-maildir)

      (evilified-state-evilify-map mu4e-headers-mode-map
        :mode mu4e-headers-mode
        :bindings
        (kbd "J") 'mu4e~headers-jump-to-maildir
        (kbd "j") 'mu4e-headers-next
        (kbd "k") 'mu4e-headers-prev)

      (evilified-state-evilify-map mu4e-view-mode-map
        :mode mu4e-view-mode
        :bindings
        (kbd "J") 'mu4e~view-headers-jump-to-maildir
        (kbd "j") 'mu4e-view-headers-next
        (kbd "k") 'mu4e-view-headers-prev)

      ;; Set variables

      (setq mu4e-use-fancy-chars t)
      (setq mu4e-headers-attach-mark (purecopy '("a" . "A")))
      (setq mu4e-headers-unread-mark (purecopy '("u" . "●")))
      (setq mu4e-hide-index-messages t)

      (setq mu4e-view-show-images t)
      (setq mu4e-view-show-addresses t)
      (setq message-kill-buffer-on-exit t)
      (setq mu4e-compose-signature-auto-include t)

      (setq mu4e-get-mail-command "mbsync -a -q")
      (setq mu4e-change-filenames-when-moving t)

      (setq smtpmail-queue-mail nil)
      (setq smtpmail-queue-dir (f-join mu4e-maildir "/queue/cur"))

      ;; Save attachments to Downloads dir.
      (setq mu4e-attachment-dir (f-expand "~/Downloads"))

      ;; Put quoted messages after signature.
      (setq message-forward-before-signature nil)

      ;; Use standard citation style.
      (setq message-citation-line-function 'message-insert-formatted-citation-line)
      (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

      ;; Update every 5 minutes.
      (setq mu4e-update-interval (* 60 5))

      ;; Disable autofill in compose buffers
      (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)

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

      (defun cb-mu4e--read-and-archive-action (docid msg target)
        ;; Must come before proc-move since retag runs 'sed' on the file
        (mu4e-action-retag-message msg "-\\Inbox")
        (-let (((_ . dest) (assoc 'mu4e-refile-folder (mu4e-context-vars (mu4e-context-current)))))
          (mu4e~proc-move docid dest "+S-u-N")))

      ;; Add read+archive mark
      (add-to-list 'mu4e-marks
                   '(read-and-archive
                     :char       "r"
                     :prompt     "rArchive"
                     :show-target (lambda (target) "archive")
                     :action      cb-mu4e--read-and-archive-action))
      (mu4e~headers-defun-mark-for read-and-archive)
      (mu4e~view-defun-mark-for read-and-archive)
      (define-key mu4e-headers-mode-map (kbd "r") 'mu4e-headers-mark-for-read-and-archive)
      (define-key mu4e-view-mode-map (kbd "r") 'mu4e-view-mark-for-read-and-archive))))

(defun cb-mu4e/init-mu4e-unread-messages ()
  (use-package mu4e-unread-messages
    :after mu4e
    :config
    (progn
      (setq display-time-use-mail-icon t)
      (setq display-time-mail-function 'mu4e-unread-messages?)
      (display-time-mode +1))))
