;;; packages.el --- cb-elisp Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-elisp-packages
  '(
    elisp-slime-nav
    cl-lib-highlight
    hl-sexp
    highlight-defined
    paredit
    eval-sexp-fu
    eldoc
    dash
    flycheck-cask
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-elisp-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t)
  (require 's nil t))

(defun cb-elisp/init-eldoc ()
  (use-package eldoc
    :diminish eldoc-mode
    :commands eldoc-mode
    :init
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))

(defun cb-elisp/init-cl-lib-highlight ()
  (use-package cl-lib-highlight
    :commands (cl-lib-highlight-initialize
               cl-lib-highlight-warn-cl-initialize)
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook 'cl-lib-highlight-initialize)
    (add-hook 'emacs-lisp-mode-hook 'cl-lib-highlight-warn-cl-initialize)))

(defun cb-elisp/init-paredit ()
  (use-package paredit
    :commands paredit-mode
    :defer t
    :init
    (add-hook 'minibuffer-setup-hook
              (lambda ()
               (when (equal this-command 'eval-expression)
                 (paredit-mode +1))))))

(defun cb-elisp/init-elisp-slime-nav ()
  (use-package elisp-slime-nav
    :commands elisp-slime-nav-mode
    :init
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)))

(defun cb-elisp/init-eval-sexp-fu ()
  (use-package eval-sexp-fu
    :commands turn-on-eval-sexp-fu-flash-mode
    :init
    (progn
      (defun cb-elisp/configure-eval-sexp-fu ()
        (core/remap-face 'eval-sexp-fu-flash 'core/bg-flash)
        (core/remap-face 'eval-sexp-fu-flash-error 'core/bg-flash-red)
        (turn-on-eval-sexp-fu-flash-mode))

      (add-hook 'emacs-lisp-mode-hook 'cb-elisp/configure-eval-sexp-fu))

    :config
    (progn
      (define-eval-sexp-fu-flash-command elisp/eval-dwim
        (eval-sexp-fu-flash
         (-let [(&plist :beg beg :end end) (elisp/thing-for-eval)]
           (cons beg end)))))))

(defun cb-elisp/init-dash ()
  (use-package dash
    :config (dash-enable-font-lock)))

(defun cb-elisp/init-flycheck-cask ()
  (use-package flycheck-cask
    :commands flycheck-cask-setup))

(defun cb-elisp/init-hl-sexp ()
  (use-package hl-sexp
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
    :config
    (core/remap-face 'hl-sexp-face 'core/bg-hl-ok)))

(defun cb-elisp/init-highlight-defined ()
  (use-package highlight-defined
    :defer t
    :commands 'highlight-defined-mode
    :init
    (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
    :config
    (progn
      (custom-set-faces
       `(highlight-defined-function-name-face
         ((((background dark))  (:foreground "#708784"))
          (((background light)) (:foreground "#708784"))))
       `(highlight-defined-builtin-function-name-face ((t (:foreground ,solarized-hl-cyan))))
       `(highlight-defined-special-form-name-face ((t (:italic t))))
       `(highlight-defined-face-name-face
         ((((background dark))  (:foreground "#8D88AE"))
          (((background light)) (:foreground "#706D84"))))))))
