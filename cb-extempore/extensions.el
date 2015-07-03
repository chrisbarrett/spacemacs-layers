;;; extensions.el --- cb-extempore Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-extempore-pre-extensions
  '(extempore)
  "List of all extensions to load before the packages.")

(defvar cb-extempore-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-extempore/init-extempore ()
  (use-package extempore
    :mode (("\\.xtm\\'" . extempore-mode))
    :commands (extempore-mode extempore-repl)
    :config
    (progn
      (setq extempore-keydef (kbd "C-c C-c"))
      (setq extempore-tab-completion nil)
      (setq user-extempore-directory "/usr/local/Cellar/extempore/0.59")
      (core/remap-face 'extempore-blink-face 'core/bg-hl-ok)
      (core/remap-face 'extempore-sb-blink-face 'core/bg-hl-ok)

      (define-key extempore-mode-map (kbd "C-c C-.") 'extempore-stop)
      (define-key extempore-mode-map (kbd "C-c C-b") 'extempore-send-buffer-or-region)
      (define-key extempore-mode-map (kbd "C-c C-f") 'extempore-send-buffer-or-region))))
