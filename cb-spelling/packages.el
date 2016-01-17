;;; packages.el --- cb-spelling Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-spelling-packages
  '(ispell
    (cb-evil-ispell :location local)))

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

(defun cb-spelling/init-cb-evil-ispell ()
  (use-package cb-evil-ispell
    :config
    (with-eval-after-load 'evil
      (evil-global-set-key 'normal (kbd "[s")  #'cb-evil-ispell-previous-spelling-error)
      (evil-global-set-key 'normal (kbd "]s")  #'cb-evil-ispell-next-spelling-error)
      (evil-global-set-key 'normal (kbd "z g") #'cb-evil-ispell-mark-word-as-good)
      (evil-global-set-key 'normal (kbd "z G") #'cb-evil-ispell-mark-word-as-locally-good)
      (evil-global-set-key 'normal (kbd "z =") #'cb-evil-ispell-correct-word)
      (evil-global-set-key 'normal (kbd "z u") #'flyspell-auto-correct-word))))
