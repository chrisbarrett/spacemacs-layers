;;; packages.el --- cb-elisp Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 's nil t)
  (require 'dash nil t))

(defconst cb-elisp-packages
  '(eldoc
    eval-sexp-fu
    evil-surround
    dash
    hl-sexp
    highlight-defined
    smart-ops
    checkdoc
    flycheck-cask
    nameless
    (elisp-autoinsert :location local)))

(use-package lisp-mode
  :config
  (progn
    (define-key emacs-lisp-mode-map (kbd "C-c C-t") #'ert)
    (define-key emacs-lisp-mode-map (kbd "M-RET")   #'elisp/M-RET)
    (define-key emacs-lisp-mode-map (kbd "C-c C-f") #'eval-buffer)
    (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'elisp/eval-dwim)
    (define-key emacs-lisp-mode-map (kbd "C-x C-e") #'pp-eval-last-sexp)

    (define-key emacs-lisp-mode-map (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)

    (spacemacs/set-leader-keys "ee" #'toggle-debug-on-error)
    (spacemacs/set-leader-keys "hfl" #'find-library)
    (spacemacs/set-leader-keys "hff" #'find-function)
    (spacemacs/set-leader-keys "hfv" #'find-variable)
    (spacemacs/set-leader-keys "hfF" #'find-face-definition)))

(defun cb-elisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(defun cb-elisp/post-init-eval-sexp-fu ()
  (defun cb-elisp/configure-eval-sexp-fu ()
    (core/remap-face 'eval-sexp-fu-flash 'cb-faces-bg-flash)
    (core/remap-face 'eval-sexp-fu-flash-error 'cb-faces-bg-flash-red)
    (turn-on-eval-sexp-fu-flash-mode))

  (add-hook 'emacs-lisp-mode-hook #'cb-elisp/configure-eval-sexp-fu)

  (define-eval-sexp-fu-flash-command elisp/eval-dwim
    (eval-sexp-fu-flash
     (-let [(&plist :beg beg :end end) (elisp/thing-for-eval)]
       (cons beg end)))))

(defun cb-elisp/post-init-dash ()
  (dash-enable-font-lock))

(defun cb-elisp/init-flycheck-cask ()
  (use-package flycheck-cask
    :commands flycheck-cask-setup))

(defun cb-elisp/init-hl-sexp ()
  (use-package hl-sexp
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
    :config
    (core/remap-face 'hl-sexp-face 'cb-faces-bg-hl-ok)))

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
       `(highlight-defined-builtin-function-name-face ((t (:foreground ,solarized-hl-cyan))))
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
  (setq checkdoc-force-docstrings-flag nil)
  (setq checkdoc-arguments-in-order-flag nil)

  ;;; HACK: Apply the above checkdoc settings for flycheck.
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
                    (kill-buffer)))))))))

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

(defun cb-elisp/init-elisp-autoinsert ()
  (use-package elisp-autoinsert
    :functions (elisp-autoinsert-init)
    :config (elisp-autoinsert-init)))

;;; packages.el ends here
