;;; extensions.el --- cb-eshell Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-eshell-pre-extensions '(eshell))

(defconst cb-eshell-post-extensions '())

(eval-when-compile
  (require 'f nil t)
  (require 'use-package nil t))

(defun cb-eshell/init-eshell ()
  (use-package eshell
    :defer t
    :init
    (progn
      (setq eshell-directory-name (f-join user-dropbox-directory "emacs/eshell/"))
      (global-set-key (kbd "<f1>") 'cb-eshell-bring))

    :config
    (progn
      (setq eshell-buffer-shorthand t)
      (setq eshell-cmpl-ignore-case t)
      (setq eshell-history-size 10000)
      (setq eshell-hist-ignoredups t)
      (setq eshell-error-if-no-glob t)
      (setq eshell-glob-case-insensitive t)
      (setq eshell-scroll-to-bottom-on-input t)
      (setq eshell-prompt-function 'cb-eshell--prompt)

      (evil-set-initial-state 'eshell-mode 'insert)

      (with-eval-after-load 'em-prompt
        (set-face-foreground 'eshell-prompt solarized-hl-cyan))

      (evil-leader/set-key-for-mode 'eshell-mode "ib" 'eshell-insert-buffer-name)
      (evil-leader/set-key-for-mode 'eshell-mode "ii" 'eshell-insert-process)
      (evil-leader/set-key-for-mode 'eshell-mode "iv" 'eshell-insert-envvar)

      (defun cb-eshell/setup ()
        (vi-tilde-fringe-mode -1)
        (local-set-key (kbd "C-c RET") 'eshell-toggle-direct-send))

      (add-hook 'eshell-mode-hook 'cb-eshell/setup)
      (add-hook 'eshell-mode-hook 'smartparens-strict-mode)

      (defun pcomplete/sudo ()
        (let ((prec (pcomplete-arg 'last -1)))
          (cond ((string= "sudo" prec)
                 (while (pcomplete-here*
                         (funcall pcomplete-command-completion-function)
                         (pcomplete-arg 'last) t)))))))))
