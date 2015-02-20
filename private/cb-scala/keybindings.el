(after 'scala-mode2
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
  (define-key ensime-mode-map (kbd "C-c C-h") 'ensime-show-doc-for-symbol-at-point)
  (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
  (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)

  (evil-leader/set-key-for-mode 'scala-mode "ii" 'ensime-import-type-at-point)

  (evil-define-key 'insert ensime-inf-mode-map
    (kbd "SPC") 'scala/smart-space
    (kbd "<backspace>") 'scala/backspace)
  )


(after 'sbt-mode
  (defun scala/sbt-ret () (interactive) (sbt:command "\n"))
  (evil-define-key 'normal sbt-mode-map (kbd "RET") 'scala/sbt-ret)
  )
