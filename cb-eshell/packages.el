;;; packages.el --- Eshell packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions)
  (require 'use-package)
  (require 'f nil t)
  (require 'pcomplete nil t))

(defconst cb-eshell-packages
  '(eshell
    company
    (eshell-prompt-extras :excluded t)
    (cb-eshell-prompt :location local)))

(defun cb-eshell/post-init-company ()
  (use-package company
    :config
    ;; Disable company for eshell.
    (setq company-global-modes '(not eshell-mode))))

(defun cb-eshell/post-init-eshell ()
  (use-package eshell
    :leader-bind
    (:mode
     eshell-mode
     ("ib" . eshell-insert-buffer-name)
     ("ii" . eshell-insert-process)
     ("iv" . eshell-insert-envvar))
    :config
    (progn
      (setq eshell-directory-name (f-join user-dropbox-directory "emacs/eshell/"))
      (setq eshell-buffer-shorthand t)
      (setq eshell-cmpl-ignore-case t)
      (setq eshell-history-size 10000)
      (setq eshell-hist-ignoredups t)
      (setq eshell-error-if-no-glob t)
      (setq eshell-glob-case-insensitive t)
      (setq eshell-scroll-to-bottom-on-input t)
      (evil-set-initial-state 'eshell-mode 'insert)

      (defun cb-eshell/ret ()
        "Do not send input if the command is empty."
        (interactive)
        (let ((empty-command? (s-matches? (concat eshell-prompt-regexp " *$") (cb-buffers-current-line))))
          (unless empty-command?
            (call-interactively #'eshell-send-input))))

      (defun cb-eshell/setup ()
        (vi-tilde-fringe-mode -1)
        (local-set-key (kbd "RET") #'cb-eshell/ret)
        (local-set-key (kbd "C-c RET") #'eshell-toggle-direct-send))

      (add-hook 'eshell-mode-hook #'cb-eshell/setup)
      (add-hook 'eshell-mode-hook #'smartparens-strict-mode)

      (defun pcomplete/sudo ()
        (let ((prec (pcomplete-arg 'last -1)))
          (cond ((string= "sudo" prec)
                 (while (pcomplete-here*
                         (funcall pcomplete-command-completion-function)
                         (pcomplete-arg 'last) t)))))))))

(defun cb-eshell/init-cb-eshell-prompt ()
  (use-package cb-eshell-prompt
    :after eshell
    :config (cb-eshell-prompt-init)))

;;; packages.el ends here
