;;; funcs.el --- Eshell functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(defun cb-eshell-bring (&optional new)
  "Display an eshell buffer, creating a new one if needed.
With prefix argument ARG, always create a new shell."
  (interactive "P")
  (cond
   (new
    (cb-eshell--new))
   ((derived-mode-p 'eshell-mode)
    (cb-eshell--hide))
   ((cb-eshell--buffer)
    (cb-eshell--show))
   (t
    (cb-eshell--new))))

(defun cb-eshell--hide ()
  (let ((reg (cb-eshell--mk-register-name)))
    (if (get-register reg)
        (or (ignore-errors (jump-to-register reg t) t)
            (bury-buffer))
      (bury-buffer)
      (when (< 1 (length (window-list)))
        (delete-window)))))

(defun cb-eshell--new ()
  (window-configuration-to-register (cb-eshell--mk-register-name))
  (save-window-excursion
    (eshell t))
  (cb-eshell--show))

(defun cb-eshell--show ()
  (window-configuration-to-register (cb-eshell--mk-register-name))
  (pop-to-buffer (cb-eshell--buffer)))

(defun cb-eshell--buffer ()
  (let ((current-frame (cb-shell--current-frame)))
    (--first (with-current-buffer it
               (and (derived-mode-p 'eshell-mode)
                    (s-matches? "eshell" (buffer-name it))
                    (equal current-frame (window-frame (get-buffer-window it)))))
             (buffer-list))))

(defun cb-shell--current-frame ()
  (window-frame (get-buffer-window (current-buffer))))

(defun cb-eshell--mk-register-name ()
  (-let [(&alist 'window-id id) (frame-parameters (cb-shell--current-frame))]
    (intern (format "cb-eshell-%s" id))))
