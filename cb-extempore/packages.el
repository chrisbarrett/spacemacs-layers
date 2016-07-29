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
    :bind
    (:map
     extempore-mode-map
     ("C-c C-." . extempore-stop)
     ("C-c C-b" . extempore-send-buffer-or-region)
     ("C-c C-f" . extempore-send-buffer-or-region))

    :config
    (progn
      (setq extempore-keydef (kbd "C-c C-c"))
      (setq extempore-tab-completion nil)
      (setq user-extempore-directory "/usr/local/Cellar/extempore/0.59")
      (cb-remap-face 'extempore-blink-face 'cb-faces-bg-hl-ok)
      (cb-remap-face 'extempore-sb-blink-face 'cb-faces-bg-hl-ok))))

;;; packages.el ends here
