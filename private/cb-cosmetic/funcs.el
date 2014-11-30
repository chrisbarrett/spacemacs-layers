;;; Color themes

(defun solarized-light ()
  "Switch theme to solarized light."
  (interactive)
  (load-theme 'solarized-light 'no-confirm))

(defun solarized-dark ()
  "Switch theme to solarized dark."
  (interactive)
  (load-theme 'solarized-dark 'no-confirm))

(defalias 'light 'solarized-light)
(defalias 'dark 'solarized-dark)


;;; Whitespace

(defun core/set-whitespace-mode ()
   "Conditionally enable whitespace mode.
In particular, disable for org src blocks so ws highlighting is not exported."
   (if (or (true? org-src-mode)
           (derived-mode-p 'haskell-mode)) ; Long lines are OK in Haskell
       (whitespace-mode +1)
     (whitespace-mode -1)))
