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
  (let ((reg (cb-shell--mk-register-name)))
    (if (get-register reg)
        (or (ignore-errors (jump-to-register reg t) t)
            (bury-buffer))
      (bury-buffer)
      (when (< 1 (length (window-list)))
        (delete-window)))))

(defun cb-shell--term-new ()
  (window-configuration-to-register (cb-shell--mk-register-name))
  (save-window-excursion
    (let ((shell (or explicit-shell-file-name (getenv "SHELL"))))
      (ansi-term shell)))
  (cb-shell--term-show))

(defun cb-shell--term-show ()
  (window-configuration-to-register (cb-shell--mk-register-name))
  (pop-to-buffer (cb-shell--term-buffer)))

(defun cb-shell--term-buffer ()
  (let ((current-frame (cb-shell--current-frame)))
    (--first (with-current-buffer it
               (and (derived-mode-p 'term-mode)
                    (s-matches? "ansi-term" (buffer-name it))
                    (equal current-frame (window-frame (get-buffer-window it)))))
             (buffer-list))))

(defun cb-shell--current-frame ()
  (window-frame (get-buffer-window (current-buffer))))

(defun cb-shell--mk-register-name ()
  (-let [(&alist 'window-id id) (frame-parameters (cb-shell--current-frame))]
    (intern (format "cb-term-%s" id))))
