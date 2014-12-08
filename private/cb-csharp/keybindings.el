(after 'csharp-mode
  (define-key omnisharp-mode-map (kbd "C-c C-c") 'omnisharp-fix-code-issue-at-point)
  (define-key omnisharp-mode-map (kbd "C-c C-t") 'omnisharp-current-type-information)
  (define-key omnisharp-mode-map (kbd "C-c C-h") 'omnisharp-current-type-documentation)

  (evil-define-key 'insert omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (evil-define-key 'insert omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
  (evil-define-key 'normal omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)

  (evil-leader/set-key-for-mode 'csharp-mode " ii" 'csharp/insert-using)
  (evil-leader/set-key-for-mode 'csharp-mode "ou"  'omnisharp-find-usages)
  (evil-leader/set-key-for-mode 'csharp-mode "oi" 'omnisharp-find-implementations) ; g i is taken
  (evil-leader/set-key-for-mode 'csharp-mode "or" 'omnisharp-run-code-action-refactoring)
  (evil-leader/set-key-for-mode 'csharp-mode "oF" 'omnisharp-fix-usings)
  (evil-leader/set-key-for-mode 'csharp-mode "oR" 'omnisharp-rename)
  (evil-leader/set-key-for-mode 'csharp-mode "oor" 'omnisharp-navigate-to-current-file-member)
  (evil-leader/set-key-for-mode 'csharp-mode "oos" 'omnisharp-navigate-to-solution-member)
  (evil-leader/set-key-for-mode 'csharp-mode "oof" 'omnisharp-navigate-to-solution-file-then-file-member)
  (evil-leader/set-key-for-mode 'csharp-mode "o." 'omnisharp-show-overloads-at-point)
  (evil-leader/set-key-for-mode 'csharp-mode "oc" 'recompile)

  (evil-leader/set-key-for-mode 'csharp-mode "oots"
    (lambda () (interactive) (omnisharp-unit-test "single")))

  (evil-leader/set-key-for-mode 'csharp-mode "ootf"
    (lambda () (interactive) (omnisharp-unit-test "fixture")))

  (evil-leader/set-key-for-mode 'csharp-mode "oott"
    (lambda () (interactive) (omnisharp-unit-test "all")))
  )
