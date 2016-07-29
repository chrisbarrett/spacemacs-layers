;;; packages.el --- cb-elisp Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions)
  (require 'use-package)
  (require 's nil t)
  (require 'dash nil t))

(defconst cb-elisp-packages
  '((lisp-mode :location built-in)
    (pp :location built-in)
    (eldoc :location built-in)
    (checkdoc :location built-in)
    (ert :location built-in)
    (find-func :location built-in)
    elisp-slime-nav
    aggressive-indent
    eval-sexp-fu
    evil-surround
    dash
    hl-sexp
    highlight-defined
    smart-ops
    flycheck
    flycheck-cask
    nameless
    (elisp-autoinsert :location local)
    (cb-elisp-meta-ret :location local)
    (cb-elisp-eval-dwim :location local)))

(defun cb-elisp/init-pp ()
  (use-package pp
    :bind
    (:map emacs-lisp-mode-map
          ("C-x C-e" . pp-eval-last-sexp))))

(defun cb-elisp/init-find-func ()
  (use-package find-func
    :leader-bind
    (("Fl" . find-library)
     ("Ff" . find-function)
     ("Fv" . find-variable)
     ("FF" . find-face-definition))))

(defun cb-elisp/post-init-elisp-slime-nav ()
  (use-package elisp-slime-nav
    :bind
    (:map emacs-lisp-mode-map
          ("M-." . elisp-slime-nav-find-elisp-thing-at-point))

    :evil-bind
    (:map emacs-lisp-mode-map
          :state normal
          ("M-." . elisp-slime-nav-find-elisp-thing-at-point)
          ("K" . elisp-slime-nav-describe-elisp-thing-at-point))))

(defun cb-elisp/init-ert ()
  (use-package ert
    :bind
    (:map emacs-lisp-mode-map
          ("C-c C-t" . ert))))

(defun cb-elisp/init-lisp-mode ()
  (use-package lisp-mode
    :mode ("Cask$" . emacs-lisp-mode)

    :bind
    (:map
     emacs-lisp-mode-map
     ("C-c C-f" . eval-buffer)
     ("C-c C-b" . eval-buffer))

    :leader-bind
    (("ee" . toggle-debug-on-error))

    :config
    (progn
      (advice-add 'eval-buffer :after
                  (lambda (&rest _)
                    (when (called-interactively-p nil)
                      (message "Buffer evaluated."))))

      (font-lock-add-keywords
       'emacs-lisp-mode
       `(
         ;; Cl keywords
         (,(rx "(" (group (or "cl-destructuring-bind"
                              "cl-case")
                          symbol-end))
          (1 font-lock-keyword-face))

         ;; Macros and functions
         (,(rx bol (* space) "("
               (group-n 1 (or "cl-defun" "cl-defmacro"
                              "cl-defstruct"
                              "cl-defsubst"
                              "cl-deftype"))
               (+ space)
               (? "(")
               (group-n 2 (+? anything) symbol-end))
          (1 font-lock-keyword-face)
          (2 font-lock-function-name-face))

         ;; General keywords
         (,(rx "(" (group (or "until"
                              "let-alist"
                              "noflet"
                              "ert-deftest"
                              "evil-global-set-keys"
                              "flycheck-declare-checker"
                              "flycheck-define-checker")
                          symbol-end))
          (1 font-lock-keyword-face))

         ;; definition forms
         (,(rx bol (* space) "("
               (group-n 1
                        symbol-start
                        (* (not (any "(" space)))
                        (or "declare" "define" "extend" "gentest")
                        (+ (not space))
                        symbol-end)
               (+ space)
               (* (any "("))
               (group-n 2 (+ (regex "\[^ )\n\]"))
                        symbol-end))
          (1 font-lock-keyword-face)
          (2 font-lock-function-name-face)))))))

(defun cb-elisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(defun cb-elisp/post-init-eval-sexp-fu ()
  (defun cb-elisp/configure-eval-sexp-fu ()
    (cb-remap-face 'eval-sexp-fu-flash 'cb-faces-bg-flash)
    (cb-remap-face 'eval-sexp-fu-flash-error 'cb-faces-bg-flash-red)
    (turn-on-eval-sexp-fu-flash-mode))

  (add-hook 'emacs-lisp-mode-hook #'cb-elisp/configure-eval-sexp-fu)

  (define-eval-sexp-fu-flash-command elisp/eval-dwim
    (eval-sexp-fu-flash
     (-let [(&plist :beg beg :end end) (elisp/thing-for-eval)]
       (cons beg end)))))

(defun cb-elisp/post-init-dash ()
  (with-eval-after-load 'dash
    (dash-enable-font-lock)))

(defun cb-elisp/init-flycheck-cask ()
  (use-package flycheck-cask
    :commands flycheck-cask-setup))

(defun cb-elisp/init-hl-sexp ()
  (use-package hl-sexp
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
    :config
    (cb-remap-face 'hl-sexp-face 'cb-faces-bg-hl-ok)))

(defun cb-elisp/init-highlight-defined ()
  (use-package highlight-defined
    :defer t
    :commands 'highlight-defined-mode
    :init
    (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
    :config
    (progn
      (custom-set-faces
       `(highlight-defined-function-name-face
         ((((background dark))  (:foreground "#708784"))
          (((background light)) (:foreground "#708784"))))
       `(highlight-defined-builtin-function-name-face ((t (:foreground ,cb-vars-solarized-hl-cyan))))
       `(highlight-defined-special-form-name-face ((t (:italic t))))
       `(highlight-defined-face-name-face
         ((((background dark))  (:foreground "#8D88AE"))
          (((background light)) (:foreground "#706D84"))))))))

(defun cb-elisp/post-init-smart-ops ()
  (define-smart-ops-for-mode 'emacs-lisp-mode
    (smart-ops "`" "," ",@"
               :pad-before t
               :pad-after nil
               :pad-unless (smart-ops-after-match? (rx (any ",{[(") eos)))
    (smart-ops "."
               :pad-before t
               :pad-after t
               :pad-unless
               (lambda (&rest _)
                 (thing-at-point-looking-at (rx digit "."))))))

(defun cb-elisp/init-checkdoc ()
  (use-package checkdoc
    :config
    (progn
      (setq checkdoc-force-docstrings-flag nil)
      (setq checkdoc-arguments-in-order-flag nil)
      ;; HACK: Apply the above checkdoc settings for flycheck.
      (with-eval-after-load 'flycheck
        (setq flycheck-emacs-lisp-checkdoc-form
              (flycheck-prepare-emacs-lisp-form
                (require 'checkdoc)
                (setq checkdoc-force-docstrings-flag nil)
                (setq checkdoc-arguments-in-order-flag nil)

                (let ((source (car command-line-args-left))
                      ;; Remember the default directory of the process
                      (process-default-directory default-directory))
                  (with-temp-buffer
                    (insert-file-contents source 'visit)
                    (setq buffer-file-name source)
                    ;; And change back to the process default directory to make file-name
                    ;; back-substutition work
                    (setq default-directory process-default-directory)
                    (with-demoted-errors "Error in checkdoc: %S"
                      (checkdoc-current-buffer t)
                      (with-current-buffer checkdoc-diagnostic-buffer
                        (princ (buffer-substring-no-properties (point-min) (point-max)))
                        (kill-buffer)))))))))))

(defun cb-elisp/init-nameless ()
  (use-package nameless
    :commands nameless-mode-from-hook
    :init
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode-from-hook)
    :config
    (progn
      (setq nameless-global-aliases
            '(("fl" . "font-lock")
              ("spacemacs" . "sm")))
      (setq nameless-prefix ":")
      (setq nameless-private-prefix t))))

(defun cb-elisp/post-init-evil-surround ()
  (use-package evil-surround
    :config
    (progn
      (defun cb-elisp/init-evil-surround-pairs ()
        (make-local-variable 'evil-surround-pairs-alist)
        (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))

      (add-hook 'emacs-lisp-mode-hook #'cb-elisp/init-evil-surround-pairs))))

(defun cb-elisp/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(defun cb-elisp/init-elisp-autoinsert ()
  (use-package elisp-autoinsert
    :functions (elisp-autoinsert-init)
    :config (elisp-autoinsert-init)))

(defun cb-elisp/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :config
    (progn
      (defun cb-elisp/maybe-enable-aggressive-indent ()
        (if (equal ".ensime" (f-filename (buffer-name)))
            (aggressive-indent-mode -1)
          (aggressive-indent-mode +1)))

      (add-hook 'emacs-lisp-mode-hook #'cb-elisp/maybe-enable-aggressive-indent))))

(defun cb-elisp/init-cb-elisp-meta-ret ()
  (use-package cb-elisp-meta-ret
    :bind
    (:map emacs-lisp-mode-map
          ("M-RET" . cb-elisp-meta-ret))))

(defun cb-elisp/init-cb-elisp-eval-dwim ()
  (use-package cb-elisp-eval-dwim
    :bind
    (:map emacs-lisp-mode-map
          ("C-c C-c" . cb-elisp-eval-dwim))))

;;; packages.el ends here
