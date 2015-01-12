;;; Show lambda symbol for lambdas.

(defvar haskell/font-lock-lambda-forms
  (list
   (core/font-lock-replace-match "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->" 1 "λ")
   (core/font-lock-replace-match (rx (group "\\") "case") 1 "λ")))

(font-lock-add-keywords 'haskell-mode haskell/font-lock-lambda-forms)
(font-lock-add-keywords 'haskell-c-mode haskell/font-lock-lambda-forms)
(font-lock-add-keywords 'haskell-interactive-mode haskell/font-lock-lambda-forms)

(defadvice haskell-mode-stylish-buffer (around suppress-window-changes activate)
  "Suppress window-changes."
  (save-window-excursion ad-do-it))


(add-to-list 'core/indent-commands-alist '(haskell-mode . haskell/format-dwim))
