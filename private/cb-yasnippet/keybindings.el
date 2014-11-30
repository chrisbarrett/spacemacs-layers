(require 'yasnippet)

(bind-key "<backspace>" 'yas/backspace yas-keymap)
(bind-key "SPC" 'yas/space yas-keymap)

(evil-global-set-key 'normal (kbd "SPC y f") 'yas/visit-snippet-file)
(evil-global-set-key 'normal (kbd "SPC y n") 'yas/new-snippet)
(evil-global-set-key 'normal (kbd "SPC y y") 'yas/insert-snippet)
(evil-global-set-key 'normal (kbd "SPC y r") 'yas//reload-all)
