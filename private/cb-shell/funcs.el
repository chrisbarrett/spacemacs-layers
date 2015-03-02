(require 'term)
(require 'dash)
(require 'f)

(defun cb-shell/ansi-term-bring (&optional force-new)
  (interactive "P")
  (let ((buf (--first (with-current-buffer it (derived-mode-p 'term-mode))
                      (buffer-list))))
    (cond
     (force-new
      (window-configuration-to-register 'term)
      (save-window-excursion (ansi-term term-ansi-default-program))
      (cb-shell/ansi-term-bring))

     ((derived-mode-p 'term-mode)
      (jump-to-register 'term t))

     (buf
      (window-configuration-to-register 'term)
      (pop-to-buffer buf))

     (t
      (cb-shell/ansi-term-bring t)))))
