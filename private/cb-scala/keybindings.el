(after 'scala-mode2
  (define-key scala-mode-map (kbd ".") nil)
  (evil-define-key 'normal scala-mode-map "J" 'scala/join-line)
  (define-key scala-mode-map (kbd "M-RET") 'scala/meta-ret)

  (define-key scala-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
  (define-key scala-mode-map (kbd "C-c C-l") 'ensime-inf-load-file)
  (define-key scala-mode-map (kbd "M-q") 'ensime-format-source)
  )

(after 'ensime
  (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src)
  )
