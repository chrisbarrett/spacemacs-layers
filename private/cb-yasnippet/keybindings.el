(require 'yasnippet)

(bind-key "<backspace>" 'yas/backspace yas-keymap)

(with-eval-after-load 'yasnippet
  (evil-define-key 'insert yas-keymap (kbd "SPC") 'yas/space))

(spacemacs/declare-prefix "y" "yasnippet")
(evil-leader/set-key "yf" 'yas-visit-snippet-file)
(evil-leader/set-key "yn" 'yas-new-snippet)
(evil-leader/set-key "yy" 'yas-insert-snippet)
(evil-leader/set-key "yr" 'yas//reload-all)
