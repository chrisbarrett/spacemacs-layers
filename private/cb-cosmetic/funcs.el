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
