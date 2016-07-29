;;; funcs.el --- Helper functions for cb-core layer -*- lexical-binding: t; -*-
;;; Documentation:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)

(autoload 'helm "helm-command")

;;; Exiting Emacs

(defun cb-core-exit-emacs ()
  (interactive)
  (let* ((emacsclient-frame? (and (not (display-graphic-p))
                                  (< 1 (length (frame-list)))))
         (prompt (if emacsclient-frame? "Finish editing? " "Kill Emacs? ")))
    (when (yes-or-no-p prompt)
      (cond
       (emacsclient-frame?
        (server-done))
       ((daemonp)
        (server-save-buffers-kill-terminal nil))
       (t
        (save-buffers-kill-emacs))))))

(defun cb-core-warn-exit-emacs-rebound ()
  (interactive)
  (user-error "Type <C-c k k> to exit Emacs"))

(defun cb-core-regexp-quoted-ignored-dirs ()
  (--map (format "/%s/" (regexp-quote it)) cb-vars-ignored-dirs))

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
