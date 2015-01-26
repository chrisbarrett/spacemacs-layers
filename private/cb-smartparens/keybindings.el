(after 'smartparens
  (sp-pair "(" ")"   :bind "M-(")
  (sp-pair "{" "}"   :bind "M-{")
  (sp-pair "[" "]"   :bind "M-[")
  (sp-pair "\"" "\"" :bind "M-\"")
  (sp-pair "`" "`"   :bind "M-`")

  (define-key sp-keymap (kbd "C-k") 'sp/kill-blank-lines)
  (define-key sp-keymap (kbd "DEL") 'sp-backward-delete-char))

(spacemacs/declare-prefix "," "smartparens")
(evil-leader/set-key
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
  )
