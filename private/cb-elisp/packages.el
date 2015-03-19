(defvar cb-elisp-packages
  '(
    ;; package elisps go here
    elisp-slime-nav
    cl-lib-highlight
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

;; For each package, define a function cb-elisp/init-<package-elisp>
;;
;; (defun cb-elisp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

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
        (turn-on-eval-sexp-fu-flash-mode)
        (add-to-list 'face-remapping-alist '(eval-sexp-fu-flash . core/bg-flash))
        (add-to-list 'face-remapping-alist '(eval-sexp-fu-flash-error . core/bg-flash-red)))

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
