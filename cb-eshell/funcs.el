;;; funcs.el --- Eshell functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

;;; Toggle display of Eshell

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


;;; Prompt

(defun eshell/host (&rest args)
  "Find the correct host name for this session."
  (or (file-remote-p default-directory 'host) system-name))

(make-variable-buffer-local 'eshell-last-command-status)

(defun cb-eshell--prompt ()
  (require 'magit)
  (let* ((status-colour (if (or (null eshell-last-command-status)
                                (= eshell-last-command-status 0))
                            solarized-hl-green
                          solarized-hl-red))
         (root-user? (= (user-uid) 0))
         (dir (abbreviate-file-name (eshell/pwd))))
    (concat "\n"
            (-if-let (hash (magit-git-string "rev-parse" "HEAD"))
                ;; Git repo info.
                (let* ((git-root (locate-dominating-file dir ".git"))
                       (repo-subdirs (f-relative dir git-root))
                       (git-str
                        (concat
                         (propertize "{" 'face font-lock-comment-face)

                         (-if-let (branch (magit-get-current-branch))
                             (propertize branch 'face 'magit-branch-remote)
                           (propertize "DETACHED" 'face `(:foreground ,solarized-hl-orange)))

                         (-when-let (tag (magit-get-current-tag))
                           (concat
                            (propertize "," 'face font-lock-comment-face)
                            (propertize tag 'face 'magit-tag)))

                         (propertize "," 'face font-lock-comment-face)
                         (propertize (substring hash 0 6) 'face 'default)
                         (propertize "}" 'face font-lock-comment-face)
                         )))
                  (concat (propertize (s-chop-suffix "/" git-root) 'face `(:bold t :foreground ,solarized-hl-blue))
                          "\n" (propertize " - " 'face font-lock-comment-face)
                          git-str
                          "\n"
                          (propertize " - " 'face font-lock-comment-face)
                          (propertize (concat "/" repo-subdirs) 'face `(:foreground ,solarized-hl-blue))))

              ;; Plain dir.
              (propertize dir 'face `(:bold t :foreground ,solarized-hl-blue)))

            "\n"
            (propertize (if root-user? ">#" ">")
                        'face `(:bold t :foreground ,status-colour))
            " ")))

(setq eshell-prompt-regexp (rx bol (* space) ">" (? "#") " "))
