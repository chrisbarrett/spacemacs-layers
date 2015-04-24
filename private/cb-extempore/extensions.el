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
    (setq extempore-keydef (kbd "C-c C-c"))))
