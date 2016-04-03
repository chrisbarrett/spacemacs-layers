;;; packages.el --- cb-spelling Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-spelling-packages
  '(ispell
    (cb-evil-ispell :location local)))

(defun cb-spelling/init-ispell ()
  (use-package ispell
    :init
    (progn
      (add-hook 'text-mode-hook #'flyspell-mode)
      (add-hook 'prog-mode-hook #'flyspell-prog-mode)
      (add-hook 'nxml-mode-hook #'flyspell-prog-mode)
      (add-hook 'sgml-mode-hook #'flyspell-prog-mode))
    :config
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-dictionary "british")
      (setq ispell-silently-savep t))))

(defun cb-spelling/init-cb-evil-ispell ()
  (use-package cb-evil-ispell
    :after evil
    :functions (cb-evil-ispell-init)
    :config (cb-evil-ispell-init)))

;;; packages.el ends here
