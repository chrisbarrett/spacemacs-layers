;;; packages.el --- cb-mu4e Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst cb-mu4e-packages '(async))

(eval-when-compile
  (require 'use-package nil t))

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
