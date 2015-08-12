;;; packages.el --- cb-cosmetic Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-cosmetic-packages
  '(
    paren-face
    whitespace
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-cosmetic-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

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
      (add-to-list 'paren-face-modes 'sml-mode)
      (add-to-list 'paren-face-modes 'extempore-mode)
      (add-to-list 'paren-face-modes 'haskell-interactive-mode)
      (add-to-list 'paren-face-modes 'idris-mode)
      (add-to-list 'paren-face-modes 'agda2-mode)
      (add-to-list 'paren-face-modes 'coq-mode))))

(defun cb-cosmetic/init-whitespace ()
  (use-package whitespace
    :diminish whitespace-mode
    :config
    (progn
      ;; HACK: override Spacemacs setting
      (remove-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

      (defadvice whitespace-turn-on (around ignore-errors activate)
        "Ignore void-function errors when starting whitespace mode."
        (condition-case _
            ad-do-it
          (void-function))))))
