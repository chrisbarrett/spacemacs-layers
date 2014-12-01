;;; Whitespace

(defun core/set-whitespace-mode ()
   "Conditionally enable whitespace mode.
In particular, disable for org src blocks so ws highlighting is not exported."
   (if (or (true? org-src-mode)
           (derived-mode-p 'haskell-mode)) ; Long lines are OK in Haskell
       (whitespace-mode +1)
     (whitespace-mode -1)))
