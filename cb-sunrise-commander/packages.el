;;; packages.el --- cb-sunrise-commander Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-sunrise-commander-packages
  '((sunrise-commander :location local)))

(defun cb-sunrise-commander/init-sunrise-commander ()
  (use-package sunrise-commander
    :config
    (progn
      (setq sr-windows-locked nil)
      (setq sr-cursor-follows-mouse nil)
      (setq sr-windows-default-ratio 33)
      (setq sr-use-commander-keys nil)
      (setq dired-auto-revert-buffer t)

      (defun cb-sunrise-commander/dired-this-dir ()
        (interactive)
        (sr-dired default-directory))

      (evil-ex-define-cmd "sr" 'sunrise)
      (evil-ex-define-cmd "sd" 'cb-sunrise-commander/dired-this-dir)

      (global-set-key (kbd "C-x d") 'sr-dired)
      (define-key sr-mode-map (kbd "J") 'sr-goto-dir)
      (define-key sr-mode-map (kbd "j") 'dired-next-line)
      (define-key sr-mode-map (kbd "k") 'dired-previous-line)
      (define-key sr-mode-map (kbd "n") 'sr-goto-dir)
      (define-key sr-mode-map (kbd "C-k") 'dired-do-kill-lines))))
