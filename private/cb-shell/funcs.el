(require 'term)
(require 'dash)

(defun cb-shell-term-bring (&optional new)
  "Display a terminal buffer, creating a new one if needed.
With prefix argument ARG, always create a new terminal."
  (interactive "P")
  (cond
   (new
    (cb-shell--term-new))
   ((derived-mode-p 'term-mode)
    (cb-shell--term-hide))
   ((cb-shell--term-buffer)
    (cb-shell--term-show))
   (t
    (cb-shell--term-new))))

(defun cb-shell--term-hide ()
  (if (get-register 'term)
      (jump-to-register 'term t)
    (bury-buffer)
    (when (< 1 (length (window-list)))
      (delete-window)))  )

(defun cb-shell--term-new ()
  (window-configuration-to-register 'term)
  (save-window-excursion
    (let ((shell (or explicit-shell-file-name (getenv "SHELL"))))
      (ansi-term shell)))
  (cb-shell--term-show))

(defun cb-shell--term-show ()
  (window-configuration-to-register 'term)
  (pop-to-buffer (cb-shell--term-buffer)))

(defun cb-shell--term-buffer ()
  (--first (with-current-buffer it
             (derived-mode-p 'term-mode))
           (buffer-list)))
