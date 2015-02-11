(after 'agda2-mode
  (evil-define-key 'normal agda2-mode-map
    ",l" 'agda2-load
    ",c" 'agda2-make-case
    ",n" 'agda2-compute-normalised
    ",g" 'agda2-give
    ",r" 'agda2-refine

    ",a" 'agda2-solveAll
    ",sc" 'agda2-show-constraints
    ",sg" 'agda2-show-goals
    ",t" 'agda2-infer-type

    ",h" 'agda2-display-implicit-arguments
    ",R" 'agda2-restart

    (kbd "M-.") 'agda2-goto-definition-keyboard
    (kbd "M-,") 'agda2-go-back
    )

  (define-key agda2-mode-map (kbd "M-RET") 'agda/meta-ret)
  (define-key agda2-mode-map (kbd "M-N") 'agda2-next-goal)
  (define-key agda2-mode-map (kbd "M-P") 'agda2-previous-goal)
  )
