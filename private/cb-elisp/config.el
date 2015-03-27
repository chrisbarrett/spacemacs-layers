(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

(defun cb-elisp/maybe-enable-aggressive-indent ()
  (if (equal ".ensime" (f-filename (buffer-name)))
      (aggressive-indent-mode -1)
    (aggressive-indent-mode +1)))


(add-hook 'emacs-lisp-mode-hook 'cb-elisp/maybe-enable-aggressive-indent)

(defadvice eval-buffer (after buffer-evaluated-feedback activate)
  "Print feedback."
  (when (called-interactively-p nil)
    (message "Buffer evaluated.")))

(custom-set-variables
 '(flycheck-emacs-lisp-load-path 'inherit))

;;; Font-lock

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
                        "let-alist"
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
