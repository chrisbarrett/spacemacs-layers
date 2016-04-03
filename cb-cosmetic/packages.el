;;; packages.el --- cb-cosmetic Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-cosmetic-packages
  '(paren-face
    whitespace
    (lambda-mode :location local)
    (cb-faces :location local)))

(defun cb-cosmetic/init-paren-face ()
  (use-package paren-face
    :commands global-paren-face-mode
    :init (global-paren-face-mode)
    :config
    (progn
      (custom-set-faces
       '(parenthesis
         ((((background light)) :foreground "grey80")
          (((background dark))  :foreground "#505070"))))

      (add-to-list 'paren-face-modes 'haskell-mode)
      (add-to-list 'paren-face-modes 'tuareg-mode)
      (add-to-list 'paren-face-modes 'utop-mode)
      (add-to-list 'paren-face-modes 'sml-mode)
      (add-to-list 'paren-face-modes 'extempore-mode)
      (add-to-list 'paren-face-modes 'haskell-interactive-mode)
      (add-to-list 'paren-face-modes 'idris-mode)
      (add-to-list 'paren-face-modes 'agda2-mode)
      (add-to-list 'paren-face-modes 'coq-mode))))

(defun cb-cosmetic/post-init-whitespace ()
  ;; HACK: override Spacemacs setting
  (remove-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

  (defadvice whitespace-turn-on (around ignore-errors activate)
    "Ignore void-function errors when starting whitespace mode."
    (condition-case _
        ad-do-it
      (void-function))))

(defun cb-cosmetic/init-lambda-mode ()
  (use-package lambda-mode
    :commands lambda-mode
    :diminish lambda-mode
    :init
    (progn
      (defvar lambda-symbol (string (make-char 'greek-iso8859-7 107)))
      (add-hook 'scheme-mode-hook        'lambda-mode)
      (add-hook 'extempore-mode-hook     'lambda-mode)
      (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
      (add-hook 'lisp-mode-hook          'lambda-mode)
      (add-hook 'emacs-lisp-mode-hook    'lambda-mode)
      (add-hook 'python-mode-hook        'lambda-mode))))

(defun cb-cosmetic/init-cb-faces ()
  (use-package cb-faces))

;;; packages.el ends here
