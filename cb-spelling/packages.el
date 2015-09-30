;;; packages.el --- cb-spelling Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-spelling-packages
  '(ispell))

(defun cb-spelling/init-ispell ()
  (use-package ispell
    :init
    (progn
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      (add-hook 'nxml-mode-hook 'flyspell-prog-mode)
      (add-hook 'sgml-mode-hook 'flyspell-prog-mode))
    :config
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-dictionary "british")
      (setq ispell-silently-savep t))))
