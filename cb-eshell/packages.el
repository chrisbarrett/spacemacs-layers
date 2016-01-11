;;; packages.el --- Eshell packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'f nil t))

(defconst cb-eshell-packages
  '(eshell
    '(eshell-prompt-extras :excluded t)))

;; Disable company for eshell.
(with-eval-after-load 'company
  (setq company-global-modes '(not eshell-mode)))

(defun cb-eshell/post-init-eshell ()
  (setq eshell-directory-name (f-join user-dropbox-directory "emacs/eshell/"))
  (global-set-key (kbd "<f1>") 'cb-eshell-bring)

  (setq eshell-buffer-shorthand t)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ignoredups t)
  (setq eshell-error-if-no-glob t)
  (setq eshell-glob-case-insensitive t)
  (setq eshell-scroll-to-bottom-on-input t)
  (setq eshell-prompt-function 'cb-eshell--prompt)

  ;; See ./funcs.el for definition.
  (setq eshell-prompt-regexp (rx bol (or
                                      ;; default
                                      (and (* space) "λ" (or ">" "#") " ")
                                      ;; boot2docker
                                      (and "@" (+ nonl) ":" (+ nonl) "$" space))))

  (evil-set-initial-state 'eshell-mode 'insert)

  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "ib" 'eshell-insert-buffer-name)
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "ii" 'eshell-insert-process)
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "iv" 'eshell-insert-envvar)

  (defun cb-eshell/ret ()
    "Do not send input if the command is empty."
    (interactive)
    (let ((empty-command? (s-matches? (concat eshell-prompt-regexp " *$") (cb-buffers-current-line))))
      (unless empty-command?
        (call-interactively 'eshell-send-input))))

  (defun cb-eshell/setup ()
    (vi-tilde-fringe-mode -1)
    (local-set-key (kbd "RET") 'cb-eshell/ret)
    (local-set-key (kbd "C-c RET") 'eshell-toggle-direct-send))

  (add-hook 'eshell-mode-hook 'cb-eshell/setup)
  (add-hook 'eshell-mode-hook 'smartparens-strict-mode)

  (defun pcomplete/sudo ()
    (let ((prec (pcomplete-arg 'last -1)))
      (cond ((string= "sudo" prec)
             (while (pcomplete-here*
                     (funcall pcomplete-command-completion-function)
                     (pcomplete-arg 'last) t)))))))

;;; packages.el ends here
