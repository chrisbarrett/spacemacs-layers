(with-eval-after-load 'scala-mode2
  (define-key scala-mode-map (kbd "M-RET") 'scala/meta-ret)
  (define-key scala-mode-map (kbd "C-c C-e") 'scala/insert-extends)

  (evil-define-key 'insert scala-mode-map
    (kbd "SPC") 'scala/smart-space
    (kbd "<backspace>") 'scala/backspace
    (kbd "<return>") 'scala/ret)
  )

(with-eval-after-load 'ensime
  (define-key ensime-inf-mode-map (kbd "C-c C-z") 'scala/switch-to-src)
  (define-key ensime-mode-map (kbd "C-c C-z") 'ensime-inf-switch)
  (define-key ensime-mode-map (kbd "C-c C-l") 'scala/load-buffer)
  (define-key ensime-mode-map (kbd "C-c C-h") 'ensime-show-doc-for-symbol-at-point)
  (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
  (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)

  (evil-define-key 'normal ensime-inspector-mode-map
    (kbd "M-.") 'ensime-inspector-browse-source
    (kbd "K") 'ensime-inspector-browse-doc
    (kbd ",") 'ensime-inspector-backward-page
    (kbd ".") 'ensime-inspector-forward-page
    (kbd "^") 'ensime-inspector-backward-page)

  (evil-define-key 'normal ensime-mode-map (kbd "M-N") 'ensime-forward-note)
  (evil-define-key 'normal ensime-mode-map (kbd "M-P") 'ensime-backward-note)
  (evil-define-key 'normal ensime-mode-map (kbd "RET") 'ensime-inspect-type-at-point)
  (evil-define-key 'normal scala-mode-map (kbd "RET") 'ensime-inspect-type-at-point)

  (evil-leader/set-key-for-mode 'scala-mode "ii" 'ensime-import-type-at-point)

  (evil-define-key 'insert ensime-inf-mode-map
    (kbd "SPC") 'scala/smart-space
    (kbd "<backspace>") 'scala/backspace)
  )


(with-eval-after-load 'sbt-mode
  (add-hook 'sbt-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'comint-clear-buffer)
              (local-set-key (kbd "C-c RET") 'scala/sbt-send-ret))))
