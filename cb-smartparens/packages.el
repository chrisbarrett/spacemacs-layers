;;; packages.el --- cb-smartparens Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-smartparens-packages
  '(smartparens))

(defun cb-smartparens/post-init-smartparens ()
  (setq sp-navigate-close-if-unbalanced t)
  (setq sp-message-width nil)

  (add-hook 'minibuffer-setup-hook 'sp/maybe-enable-smartparens t)
  (add-hook 'minibuffer-inactive-mode-hook 'sp/maybe-enable-smartparens t)

  (add-hook 'smartparens-mode-hook 'sp/hacky-set-sp-bindings t)
  (add-hook 'smartparens-strict-mode-hook 'sp/hacky-set-sp-bindings t)

  (smartparens-global-strict-mode +1)
  (show-smartparens-global-mode +1)

  ;; Keybindings

  (evil-global-set-key 'insert (kbd "DEL") 'sp-backward-delete-char)

  (evil-define-key 'insert prog-mode-map
    (kbd "<backspace>") 'sp-generic-prog-backspace
    (kbd "DEL") 'sp-generic-prog-backspace
    (kbd "SPC") 'sp-generic-prog-space
    (kbd "RET") 'sp-generic-prog-ret)

  (sp-pair "(" ")"   :bind "M-(")
  (sp-pair "{" "}"   :bind "M-{")
  (sp-pair "[" "]"   :bind "M-[")
  (sp-pair "\"" "\"" :bind "M-\"")
  (sp-pair "`" "`"   :bind "M-`")

  (define-key sp-keymap (kbd "C-k") 'sp/kill-blank-lines)

  (spacemacs/declare-prefix "," "smartparens")

  (spacemacs/set-leader-keys
    ",A" 'sp-add-to-previous-sexp
    ",a" 'sp-add-to-next-sexp
    ",B" 'sp-backward-barf-sexp
    ",b" 'sp-forward-barf-sexp
    ",M" 'sp-backward-slurp-sexp
    ",m" 'sp-forward-slurp-sexp
    ",c" 'sp-convolute-sexp
    ",D" 'sp-backward-kill-sexp
    ",d" 'sp-kill-sexp
    ",e" 'sp-emit-sexp
    ",l" 'sp-end-of-sexp
    ",h" 'sp-beginning-of-sexp
    ",j" 'sp-join-sexp
    ",K" 'sp-splice-sexp-killing-backward
    ",k" 'sp-splice-sexp-killing-forward
    ",n" 'sp-next-sexp
    ",p" 'sp-previous-sexp
    ",r" 'sp-raise-sexp
    ",s" 'sp-splice-sexp-killing-around
    ",t" 'sp-transpose-sexp
    ",U" 'sp-backward-unwrap-sexp
    ",u" 'sp-unwrap-sexp
    ",w" 'sp-rewrap-sexp
    ",x" 'sp-split-sexp
    ",Y" 'sp-backward-copy-sexp
    ",y" 'sp-copy-sexp
    ",," 'sp-previous-sexp
    ",." 'sp-next-sexp
    ",<" 'sp-backward-down-sexp
    ",>" 'sp-down-sexp
    ))

;;; packages.el ends here
