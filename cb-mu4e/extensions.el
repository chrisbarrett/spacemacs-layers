;;; extensions.el --- cb-mu4e Layer extensions File for Spacemacs

;;; Commentary:

;; Mu4e mail client configuration. The accounts themselves are configured in my
;; personal config.
;;
;; There is a hard dependency between mu4e and the mu version so it is assumed
;; to have been installed via a package manager.
;;
;; On OS X:
;;
;;   brew install mu --with-emacs
;;

;;; Code:

(defconst cb-mu4e-pre-extensions '(mu4e mu4e-multi))

(defconst cb-mu4e-post-extensions '())

(eval-when-compile
  (require 'use-package nil t)
  (require 'dash)
  (require 'f))

;; Ensure extensions are on the load-path.
(--each (f-directories (f-join user-layers-directory "cb-mu4e/extensions"))
  (add-to-list 'load-path (f-slash it)))

(defun cb-mu4e/init-mu4e ()
  (use-package mu4e
    :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
    :init
    (evil-leader/set-key "am" 'mu4e)
    :config
    (progn

      (define-key mu4e-headers-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir)
      (define-key mu4e-headers-mode-map (kbd "j") 'mu4e-headers-next)
      (define-key mu4e-headers-mode-map (kbd "k") 'mu4e-headers-prev)

      (define-key mu4e-view-mode-map (kbd "J") 'mu4e~view-headers-jump-to-maildir)
      (define-key mu4e-view-mode-map (kbd "j") 'mu4e-view-headers-next)
      (define-key mu4e-view-mode-map (kbd "k") 'mu4e-view-headers-prev)

      (setq mu4e-use-fancy-chars t)
      (setq mu4e-headers-attach-mark (purecopy '("a" . "📎")))

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

      ;; Update every 5 minutes.
      (setq mu4e-update-interval (* 60 5))

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

      ;; Define generic browser functions to support mu4e.
      (setq browse-url-browser-function 'eww-browse-url)
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

      (add-hook 'mu4e-compose-mode-hook 'cb-mu4e-flow-text))))

(defun cb-mu4e/init-mu4e-multi ()
  (use-package mu4e-multi
    :config
    (progn
      (global-set-key (kbd "C-x m") 'mu4e-multi-compose-new)
      ;; See `mu4e-multi-account-alist' in personal-config file for accounts
      ;; configuration (not on GitHub).
      (with-eval-after-load 'personal-config (mu4e-multi-enable)))))
