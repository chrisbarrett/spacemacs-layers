;;; packages.el --- cb-sunrise-commander Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst cb-sunrise-commander-packages
  '((sunrise-commander :location local)))

(defun cb-sunrise-commander/init-sunrise-commander ()
  (use-package sunrise-commander
    :commands sunrise
    :bind
    (("C-x d" . sr-dired)
     :map sr-mode-map
     ("J" . sr-goto-dir)
     ("j" . dired-next-line)
     ("k" . dired-previous-line)
     ("n" . sr-goto-dir)
     ("C-k" . dired-do-kill-lines))
    :init
    (progn
      (defun cb-sunrise-commander/dired-this-dir ()
        (interactive)
        (sr-dired default-directory))

      (evil-ex-define-cmd "sr" #'sunrise)
      (evil-ex-define-cmd "sd" #'cb-sunrise-commander/dired-this-dir))

    :config
    (progn
      (setq sr-windows-locked nil)
      (setq sr-cursor-follows-mouse nil)
      (setq sr-windows-default-ratio 33)
      (setq sr-use-commander-keys nil)
      (setq dired-auto-revert-buffer t))))
