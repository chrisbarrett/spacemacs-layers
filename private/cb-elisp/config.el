(autoload 'paredit-mode "paredit")
(require 'eval-sexp-fu)
(require 'cl-lib-highlight)

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when (equal this-command 'eval-expression)
              (paredit-mode +1))))


(add-hook 'emacs-lisp-mode-hook 'cl-lib-highlight-initialize)
(add-hook 'emacs-lisp-mode-hook 'cl-lib-highlight-warn-cl-initialize)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)


(defadvice eval-buffer (after buffer-evaluated-feedback activate)
  "Print feedback."
  (when (called-interactively-p nil)
    (message "Buffer evaluated.")))


;;; eval-sexp-fu

(add-to-list 'face-remapping-alist '(eval-sexp-fu-flash . core/bg-flash))
(add-to-list 'face-remapping-alist '(eval-sexp-fu-flash-error . core/bg-flash-red)) 

(define-eval-sexp-fu-flash-command elisp/eval-dwim
  (eval-sexp-fu-flash
   (cl-destructuring-bind (&key beg end &allow-other-keys) (elisp/thing-for-eval)
     (cons beg end))))


;;; Font-lock

(dash-enable-font-lock)

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
                        "hook-fn"
                        "hook-fns"
                        "lambda+"
                        "after"
                        "noflet"
                        "ert-deftest"
                        "ac-define-source"
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
    (2 font-lock-function-name-face)))
 )
