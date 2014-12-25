(after 'scala-mode2
  (define-key scala-mode-map (kbd ".") nil)
  (evil-define-key 'normal scala-mode-map "J" 'scala/join-line)
  (define-key scala-mode-map (kbd "M-RET") 'scala/meta-ret)

  (define-key scala-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
  (define-key scala-mode-map (kbd "C-c C-l") 'ensime-inf-load-file)
  (define-key scala-mode-map (kbd "M-q") 'ensime-format-source)

  (evil-define-key 'insert scala-mode-map (kbd "SPC") 'scala/smart-space)
  (evil-define-key 'insert scala-mode-map (kbd "<backspace>") 'scala/backspace)
  (evil-define-key 'insert scala-mode-map (kbd "<return>") 'scala/ret)
  )

(after 'ensime
  (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src)
  (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
  (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)

  (evil-define-key 'insert ensime-inf-mode-map (kbd "SPC") 'scala/smart-space)
  (evil-define-key 'insert ensime-inf-mode-map (kbd "<backspace>") 'scala/backspace)
  )
