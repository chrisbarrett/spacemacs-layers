;;; funcs.el --- Helper functions for cb-core layer -*- lexical-binding: t; -*-
;;; Documentation:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)

(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'helm "helm-command")
(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")
(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'recentf-cleanup "recentf")

;;; File utils

(defun cb-core-rename-file-and-buffer (buffer dest-dir dest-filename)
  "Rename the current buffer and file it is visiting."
  (interactive (let ((cur (cb-core--assert-file-exists-for-buffer)))
                 (list (current-buffer)
                       (read-directory-name "Move to directory: " (f-dirname cur))
                       (read-string "New name: " (f-filename cur)))))
  (let ((src (cb-core--assert-file-exists-for-buffer buffer))
        (dest-path (f-join dest-dir dest-filename)))
    (or (cb-core--try-move-file-with-vc src dest-path)
        (cb-core--try-rename-file src dest-path))
    (message "File '%s' moved to '%s'" (f-short (f-filename src)) (f-short dest-path))))

(defun cb-core--assert-file-exists-for-buffer (&optional buf)
  (let ((cur (buffer-file-name buf)))
    (if (not (and cur (f-exists? cur)))
        (error "Buffer is not visiting a file!")
      cur)))

(defun cb-core--try-move-file-with-vc (src dest)
  (condition-case err
      (when (vc-backend src)
        (vc-rename-file src dest)
        t)
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((s-matches? "New file already exists" msg) nil)
        ((s-matches? "Please update files" msg)
         (unless (y-or-n-p "VC cannot track this change automatically. Continue?")
           (error msg)))
        (t
         (error msg)))))))

(defun cb-core--try-rename-file (src dest)
  (when (and (f-exists? dest) (not (y-or-n-p "File exists. Overwrite?")))
    (user-error "Aborted"))
  (rename-file src dest t)
  (-when-let (buf (get-file-buffer src))
    (with-current-buffer buf
      (rename-buffer dest)
      (set-visited-file-name dest)
      (set-buffer-modified-p nil))

    (recentf-cleanup)
    (when (projectile-project-p)
      (projectile-invalidate-cache nil))))

(defun cb-core-move-file (buffer to-dir)
  "Move BUFFER's corresponding file to DEST."
  (interactive (list (current-buffer) (read-directory-name "Move to: ")))
  (let ((current-file-name (f-filename (cb-core--assert-file-exists-for-buffer buffer))))
    (cb-core-rename-file-and-buffer buffer to-dir current-file-name)))

;;; Window management

(defun cb-core--rotate-window-layout ()
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                                     (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
                 (car (window-edges (next-window))))
              'split-window-horizontally
            'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (when this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (when this-win-2nd (other-window 1)))))

(defun cb-core-toggle-window-split ()
  (interactive)
  (cond
   ((= (count-windows) 2)
    (cb-core--rotate-window-layout))
   ((= 1 (count-windows))
    (user-error "No windows to rotate"))
   (t
    (user-error "Too many windows to rotate"))))

(defun cb-core-move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun cb-core-move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

;;; Exiting Emacs

(defun cb-core-exit-emacs ()
  (interactive)
  (when (yes-or-no-p "Kill Emacs? ")
    (if (daemonp)
        (server-save-buffers-kill-terminal nil)
      (save-buffers-kill-emacs))))

(defun cb-core-warn-exit-emacs-rebound ()
  (interactive)
  (user-error "Type <C-c k k> to exit Emacs"))

(defun cb-core-regexp-quoted-ignored-dirs ()
  (--map (format "/%s/" (regexp-quote it)) cb-vars-ignored-dirs))

(defun cb-core-ansi-colourise-compilation ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun cb-core-font-lock-replace-match (regex group replacement)
  "Return a font-lock replacement spec for.

REGEX surrounds the text to be replaced with a group.

GROUP is the number of the group.

REPLACEMENT is the string to substitute for the match in REGEX."
  (list regex
        `(0 (progn (compose-region (match-beginning ,group) (match-end ,group)
                                   ,replacement 'decompose-region)
                   nil))))

;;; Global insertion commands

(defun cb-core-generate-password ()
  (interactive)
  (kill-new (s-trim (shell-command-to-string "gpg --gen-random --armor 1 30")))
  (message "Password copied to kill-ring."))

;;; HACK: override Spacemacs function to prevent M-RET from being bound.

(defun spacemacs/activate-major-mode-leader ()
  "Bind major mode key map to `dotspacemacs-major-mode-leader-key'."
  (setq mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
  (when mode-map
    (setq major-mode-map (lookup-key mode-map (kbd "m")))
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-local-map" s))
                     ,(kbd dotspacemacs-major-mode-leader-key)
                     major-mode-map)))
          '(normal motion))))

;;; funcs.el ends here
