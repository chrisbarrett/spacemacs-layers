(require 'smartparens)

(sp-pair "(" ")"   :bind "M-(")
(sp-pair "{" "}"   :bind "M-{")
(sp-pair "[" "]"   :bind "M-[")
(sp-pair "\"" "\"" :bind "M-\"")
(sp-pair "`" "`"   :bind "M-`")

(define-key sp-keymap (kbd "C-k") 'sp/kill-blank-lines)
(define-key sp-keymap (kbd "DEL") 'sp-backward-delete-char)
