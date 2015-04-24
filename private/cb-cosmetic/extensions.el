;;; extensions.el --- cb-cosmetic Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-cosmetic-pre-extensions
  '(lambda-mode)
  "List of all extensions to load before the packages.")

(defvar cb-cosmetic-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-cosmetic/init-lambda-mode ()
  (use-package lambda-mode
    :commands lambda-mode
    :diminish lambda-mode
    :init
    (progn
      (defvar lambda-symbol (string (make-char 'greek-iso8859-7 107)))
      (add-hook 'scheme-mode-hook        'lambda-mode)
      (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
      (add-hook 'lisp-mode-hook          'lambda-mode)
      (add-hook 'emacs-lisp-mode-hook    'lambda-mode)
      (add-hook 'python-mode-hook        'lambda-mode))))
