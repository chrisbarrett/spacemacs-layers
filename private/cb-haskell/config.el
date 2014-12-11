;;; Show lambda symbol for lambdas.

(defvar haskell/font-lock-lambdas-form
  `(("\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->"
     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                               ,(string-to-char "λ") 'decompose-region)
               nil)))))

(font-lock-add-keywords 'haskell-mode haskell/font-lock-lambdas-form)
(font-lock-add-keywords 'haskell-c-mode haskell/font-lock-lambdas-form)
(font-lock-add-keywords 'haskell-interactive-mode haskell/font-lock-lambdas-form)

(defadvice haskell-mode-stylish-buffer (around suppress-window-changes activate)
  "Suppress window-changes."
  (save-window-excursion ad-do-it))
