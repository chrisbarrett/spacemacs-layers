(after 'haskell-mode
  (evil-define-key 'normal haskell-mode-map (kbd "SPC i i") 'haskell/insert-import)
  (evil-define-key 'normal haskell-mode-map (kbd "SPC i q") 'haskell/insert-qualified-import)
  (evil-define-key 'normal haskell-mode-map (kbd "SPC i l") 'haskell/insert-language-pragma)
  (evil-define-key 'normal haskell-mode-map (kbd "SPC i o") 'haskell/insert-ghc-option)

  (evil-define-key 'normal haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  (define-key haskell-mode-map (kbd "M-,")           'pop-tag-mark)
  (define-key haskell-mode-map (kbd "M-P")           'flymake-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-N")           'flymake-goto-next-error)
  (define-key haskell-mode-map (kbd "C-,")           'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.")           'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "C-c C-d")       'haskell-w3m-open-haddock)
  (define-key haskell-mode-map (kbd "C-c C-f")       'haskell-cabal-visit-file)
  (define-key haskell-mode-map (kbd "C-c C-h")       'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c C-c")       'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k")       'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "M-q")           'haskell/format-dwim)
  (define-key haskell-mode-map (kbd "M-RET")         'haskell/meta-ret)
  (define-key haskell-mode-map (kbd "<backspace>")   'haskell/backspace)

  (define-key haskell-mode-map (kbd "#") 'haskell/smart-hash)
  )

(after 'haskell-cabal-mode
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  )

(after 'haskell-interactive-mode
  (define-key haskell-interactive-mode-map (kbd "C-c C-h") 'haskell-hoogle)
  (evil-define-key 'normal haskell-error-mode-map (kbd "q") 'quit-window))

(after 'shm
  (evil-define-key 'normal shm-map "J" 'haskell/join-line)

  (define-key shm-map (kbd "C-k")        'shm/kill-node)
  (define-key shm-map (kbd "C-c C-s")    'shm/case-split)
  (define-key shm-map (kbd "C-<return>") 'shm/newline-indent)
  (define-key shm-map (kbd "SPC") 'haskell/smart-space)

  ;; undefine commands that interfere with smart ops, etc.
  (define-key shm-map (kbd "RET") nil)
  (define-key shm-map (kbd ",") nil)
  (define-key shm-map (kbd ":") nil)
  (define-key shm-map (kbd "#") nil)
  (define-key shm-map (kbd "-") nil)
  (define-key shm-map (kbd "DEL") nil)
  (define-key shm-map (kbd "C-<backspace>") nil)
  (define-key shm-map (kbd "<backtab>") nil)
  (define-key shm-map (kbd "TAB") nil)
  (define-key shm-map (kbd "M-r") nil)
  )
