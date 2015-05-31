;;; extensions.el --- cb-sunrise-commander Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-sunrise-commander-pre-extensions
  '(sunrise-commander))

(defconst cb-sunrise-commander-post-extensions
  '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-sunrise-commander/init-sunrise-commander ()
  (use-package sunrise-commander
    :config
    (progn
      (evil-ex-define-cmd "sr" 'sr-dired)

      (define-key sr-mode-map (kbd "j") 'dired-next-line)
      (define-key sr-mode-map (kbd "k") 'dired-previous-line)
      (define-key sr-mode-map (kbd "n") 'sr-goto-dir)
      (define-key sr-mode-map (kbd "C-k") 'dired-do-kill-lines))))
