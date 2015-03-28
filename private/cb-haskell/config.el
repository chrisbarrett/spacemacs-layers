(setq haskell-enable-ghci-ng-support t)
(setq haskell-enable-hindent-style "gibiansky")
(setq haskell-enable-shm-support t)


;;; Font locking

(defun haskell/apply-font-locking ()
  (font-lock-add-keywords
   nil
   `((,(rx (or bol space "}" ")" "]")
           (group "$" (? "!"))
           (or eol space "{" "(" "["))
      1 'font-lock-comment-face)

     ("∀" . font-lock-keyword-face)

     ,(core/font-lock-replace-match (rx (or (and space (group-n 1 ".") space)
                                            (and "(" (group-n 1 ".") ")")
                                            ))
                                    1 "·")

     ;; Lambda forms
     ,(core/font-lock-replace-match "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->" 1 "λ")
     ,(core/font-lock-replace-match "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *→" 1 "λ")
     ,(core/font-lock-replace-match (rx (group "\\") "case") 1
                                    (propertize "λ" 'face 'font-lock-add-keywords)))))

(add-hook 'haskell-mode-hook 'haskell/apply-font-locking)
(add-hook 'haskell-c-mode-hook 'haskell/apply-font-locking)
(add-hook 'haskell-interactive-mode-hook 'haskell/apply-font-locking)


(custom-set-faces
 `(haskell-interactive-face-compile-error ((t (:foreground nil)))))

(defun haskell/interactive-apply-font-locking ()
  (font-lock-add-keywords
   nil
   `(("Compilation failed." . '(face nil :foreground compilation-error-face)))))

(add-hook 'haskell-interactive-mode-hook 'haskell/interactive-apply-font-locking)

(defadvice haskell-mode-stylish-buffer (around suppress-window-changes activate)
  "Suppress window-changes."
  (save-window-excursion ad-do-it))

(add-to-list 'core/indent-commands-alist '(haskell-mode . haskell/format-dwim))

(defalias 'ghci 'haskell-interactive-switch)
