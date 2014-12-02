(require 'yasnippet)

(bind-key "<backspace>" 'yas/backspace yas-keymap)
(bind-key "SPC" 'yas/space yas-keymap)

(evil-leader/set-key "yf" 'yas/visit-snippet-file)
(evil-leader/set-key "yn" 'yas/new-snippet)
(evil-leader/set-key "yy" 'yas/insert-snippet)
(evil-leader/set-key "yr" 'yas//reload-all)
