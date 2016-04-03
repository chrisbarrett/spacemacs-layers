;;; packages.el --- Packages required for extempore.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-extempore-packages
  '(osc
    (extempore :location local)))

(defun cb-extempore/init-osc ()
  (use-package osc
    :defer t))

(defun cb-extempore/init-extempore ()
  (use-package extempore
    :mode (("\\.xtm\\'" . extempore-mode))
    :commands (extempore-mode extempore-repl)
    :config
    (progn
      (with-no-warnings
        (setq extempore-keydef (kbd "C-c C-c"))
        (setq extempore-tab-completion nil)
        (setq user-extempore-directory "/usr/local/Cellar/extempore/0.59"))

      (core/remap-face 'extempore-blink-face 'cb-faces-bg-hl-ok)
      (core/remap-face 'extempore-sb-blink-face 'cb-faces-bg-hl-ok)

      (with-no-warnings
        (define-key extempore-mode-map (kbd "C-c C-.") #'extempore-stop)
        (define-key extempore-mode-map (kbd "C-c C-b") #'extempore-send-buffer-or-region)
        (define-key extempore-mode-map (kbd "C-c C-f") #'extempore-send-buffer-or-region)))))

;;; packages.el ends here
