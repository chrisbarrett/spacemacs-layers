(after 'scala-mode2
  ;; (define-key scala-mode-map (kbd ".") nil)
  (define-key scala-mode-map (kbd "M-RET") 'scala/meta-ret)
  (define-key scala-mode-map (kbd "C-c C-e") 'scala/insert-extends)

  (evil-define-key 'insert scala-mode-map
    (kbd "SPC") 'scala/smart-space
    (kbd "<backspace>") 'scala/backspace
    (kbd "<return>") 'scala/ret)
  )

(after 'ensime
  (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src)
  (define-key ensime-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
  (define-key ensime-mode-map (kbd "C-c C-l") 'scala/load-buffer)
  (define-key ensime-mode-map (kbd "C-c C-r C-r") 'ensime-refactor-rename)
  (define-key ensime-mode-map (kbd "C-c C-h") 'ensime-show-doc-for-symbol-at-point)
  (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
  (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)

  (evil-leader/set-key-for-mode 'scala-mode "ii" 'ensime-import-type-at-point)

  (evil-define-key 'insert ensime-inf-mode-map
    (kbd "SPC") 'scala/smart-space
    (kbd "<backspace>") 'scala/backspace)

  (evil-define-key 'insert ensime-mode-map
    (kbd "M-.") 'ensime-edit-definition
    (kbd "M-,") 'ensime-pop-find-definition-stack)

  (evil-define-key 'normal ensime-mode-map
    (kbd "M-.") 'ensime-edit-definition
    (kbd "M-,") 'ensime-pop-find-definition-stack)

  (evil-define-key 'normal ensime-inspector-mode-map
    (kbd "q") 'ensime-popup-buffer-quit-function)

  (evil-define-key 'normal ensime-mode-map
    (kbd ",t") 'scala/tests-watch
    (kbd ",o") 'scala/test-only-watch
    (kbd ",i") 'ensime-inspect-type-at-point
    (kbd ",I") 'ensime-import-type-at-point
    (kbd ",p") 'ensime-inspect-project-package
    (kbd ",c") 'ensime-typecheck-current-file
    (kbd ",C") 'ensime-typecheck-all

    (kbd ",u") 'ensime-show-uses-of-symbol-at-point
    (kbd ",r") 'ensime-refactor-rename
    (kbd ",l") 'ensime-refactor-extract-local
    (kbd ",L") 'ensime-refactor-inline-local

    (kbd ",s") 'ensime-sbt-switch
    )

  (evil-define-key 'visual ensime-mode-map
    (kbd ",m") 'ensime-refactor-extract-method
    (kbd ",l") 'ensime-refactor-extract-local)
  )
