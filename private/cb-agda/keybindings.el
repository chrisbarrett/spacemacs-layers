(after 'agda2-mode
  (evil-define-key 'normal agda2-mode-map
    ",l" 'agda2-load
    ",c" 'agda2-compute-normalised
    ",C" 'agda2-compile
    ",d" 'agda2-infer-type
    ",g" 'agda2-show-goals
    ",h" 'agda2-display-implicit-arguments
    ",n" 'agda2-next-goal
    ",N" 'agda2-previous-goal
    ",s" 'agda2-show-constraints
    ",S" 'agda2-solveAll
    ",x" 'agda2-restart)

  (define-key agda2-mode-map (kbd "M-RET") 'agda/meta-ret)
  (define-key agda2-mode-map (kbd "M-N") 'agda2-next-goal)
  (define-key agda2-mode-map (kbd "M-P") 'agda2-previous-goal)
  )
