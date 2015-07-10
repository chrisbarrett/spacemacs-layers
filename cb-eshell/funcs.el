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
  (let ((prompt
         (list
          :directory (abbreviate-file-name (eshell/pwd))
          :root-user? (= (user-uid) 0)
          :git-hash (magit-git-string "rev-parse" "HEAD")
          :git-tag  (magit-get-current-tag)
          :git-root (locate-dominating-file (eshell/pwd) ".git")
          :git-branch (magit-get-current-branch)
          :git-staged? (magit-anything-staged-p)
          :git-unmerged? (magit-anything-unmerged-p)
          :git-untracked? (ignore-errors
                            (not (s-blank? (s-trim (shell-command-to-string "git ls-files --other --directory --exclude-standard")))))
          :git-unstaged? (magit-anything-staged-p)
          :git-modified? (magit-anything-modified-p)
          :last-command-success? (when (boundp 'eshell-last-command-status)
                                   (or (null eshell-last-command-status)
                                       (= eshell-last-command-status 0))))))
    (prog1 (cb-eshell--render-prompt prompt)
      (setq cb-eshell--last-prompt prompt))))

(defvar-local cb-eshell--last-prompt nil)

(defun cb-eshell--render-prompt (plist)
  (-let [(&plist :last-command-success? last-command-success?
                 :root-user? root-user?
                 ) plist]
    (concat
     "\n"
     (unless (equal plist cb-eshell--last-prompt)
       (concat "\n" (cb-eshell--render-header plist) "\n"))

     (let ((colour (if last-command-success? solarized-hl-green solarized-hl-red)))
       (propertize (if root-user? ">#" ">") 'face `(:bold t :foreground ,colour)))

     " "))  )

(defun cb-eshell--render-header (plist)
  (-let [(&plist
          :git-branch git-branch
          :git-hash git-hash
          :git-tag git-tag
          :git-root git-root
          :directory dir
          :git-staged? git-staged?
          :git-unstaged? git-unstaged?
          :git-unmerged? git-unmerged?
          :git-untracked? git-untracked?
          :git-modified? git-modified?
          ) plist]
    (if git-hash
        ;; Git repo info.
        (let ((repo-subdirs (f-relative dir git-root)))
          (concat (propertize (s-chop-suffix "/" git-root) 'face `(:foreground ,solarized-hl-blue))
                  "\n" (propertize " - " 'face font-lock-comment-face)

                  (if git-branch
                      (concat
                       (propertize "branch:" 'face font-lock-comment-face)
                       (propertize git-branch 'face 'magit-branch-remote))
                    (propertize "DETACHED" 'face `(:foreground ,solarized-hl-orange)))

                  (when git-tag
                    (concat
                     (propertize " tag:" 'face font-lock-comment-face)
                     (propertize git-tag 'face 'magit-tag)))

                  (propertize " sha:" 'face font-lock-comment-face)
                  (propertize (substring git-hash 0 6) 'face 'default)

                  (when (or git-staged? git-unstaged? git-modified?)
                    (concat
                     (propertize " state:{" 'face font-lock-comment-face)
                     (s-join " "
                             (-keep 'identity
                                    (list
                                     (when git-staged? (propertize "staged" 'face `(:foreground ,solarized-hl-green)))
                                     (when git-unstaged? (propertize "unstaged" 'face `(:foreground ,solarized-hl-cyan)))
                                     (when git-unmerged? (propertize "unmerged" 'face `(:foreground ,solarized-hl-magenta)))
                                     (when git-modified? (propertize "modified" 'face `(:foreground ,solarized-hl-red)))
                                     (when git-untracked? (propertize "untracked" 'face 'default)))))
                     (propertize "}" 'face font-lock-comment-face)))

                  "\n"
                  (propertize " - " 'face font-lock-comment-face)
                  (propertize (concat "/" repo-subdirs) 'face `(:foreground ,solarized-hl-blue))
                  "\n"))

      ;; Plain dir.
      (propertize dir 'face `(:bold t :foreground ,solarized-hl-blue)))))

(setq eshell-prompt-regexp (rx bol (* space) ">" (? "#") " "))
