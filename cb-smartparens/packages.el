;;; packages.el --- cb-smartparens Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'smartparens nil t)
  (require 'use-package nil t))

(autoload 'evil-global-set-key "evil-core")
(autoload 'smartparens-global-strict-mode "smartparens")
(autoload 'sp-pair "smartparens")
(autoload 'spacemacs/declare-prefix "core-keybindings")
(autoload 'spacemacs/set-leader-keys "core-keybindings")

(defconst cb-smartparens-packages
  '(smartparens
    (sp-generic-prog :location local)
    (sp-in-minibuffer :location local)
    (sp-insert-or-up :location local)))

(defun cb-smartparens/post-init-smartparens ()
  (use-package smartparens
    :config
    (progn
      (setq sp-navigate-close-if-unbalanced t)
      (setq sp-message-width nil)
      (smartparens-global-strict-mode +1)
      (show-smartparens-global-mode +1)

      (evil-global-set-key 'insert (kbd "DEL") #'sp-backward-delete-char)

      (sp-pair "(" ")"   :bind "M-(")
      (sp-pair "{" "}"   :bind "M-{")
      (sp-pair "[" "]"   :bind "M-[")
      (sp-pair "\"" "\"" :bind "M-\"")
      (sp-pair "`" "`"   :bind "M-`")

      (spacemacs/declare-prefix "," "smartparens")

      (spacemacs/set-leader-keys
        ",A" #'sp-add-to-previous-sexp
        ",a" #'sp-add-to-next-sexp
        ",B" #'sp-backward-barf-sexp
        ",b" #'sp-forward-barf-sexp
        ",M" #'sp-backward-slurp-sexp
        ",m" #'sp-forward-slurp-sexp
        ",c" #'sp-convolute-sexp
        ",D" #'sp-backward-kill-sexp
        ",d" #'sp-kill-sexp
        ",e" #'sp-emit-sexp
        ",l" #'sp-end-of-sexp
        ",h" #'sp-beginning-of-sexp
        ",j" #'sp-join-sexp
        ",K" #'sp-splice-sexp-killing-backward
        ",k" #'sp-splice-sexp-killing-forward
        ",n" #'sp-next-sexp
        ",p" #'sp-previous-sexp
        ",r" #'sp-raise-sexp
        ",s" #'sp-splice-sexp-killing-around
        ",t" #'sp-transpose-sexp
        ",U" #'sp-backward-unwrap-sexp
        ",u" #'sp-unwrap-sexp
        ",w" #'sp-rewrap-sexp
        ",x" #'sp-split-sexp
        ",Y" #'sp-backward-copy-sexp
        ",y" #'sp-copy-sexp
        ",," #'sp-previous-sexp
        ",." #'sp-next-sexp
        ",<" #'sp-backward-down-sexp
        ",>" #'sp-down-sexp))))

(defun cb-smartparens/init-sp-generic-prog ()
  (use-package sp-generic-prog
    :functions (sp-generic-prog-init)
    :init (add-hook 'prog-mode-hook #'sp-generic-prog-init)))

(defun cb-smartparens/init-sp-in-minibuffer ()
  (use-package sp-in-minibuffer
    :functions (sp-in-minibuffer-init)
    :config (sp-in-minibuffer-init)))

(defun cb-smartparens/init-sp-insert-or-up ()
  (use-package sp-insert-or-up
    :functions (sp-insert-or-up-init)
    :config (sp-insert-or-up-init)))

;;; packages.el ends here
