;;; funcs.el --- Functions supporting git config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 's nil t)
  )

(defun git/browse-repo (dir)
  (interactive
   (list (or (projectile-project-p)
             (projectile-completing-read "Project: " (projectile-relevant-known-projects)))))
  (unless (magit-git-repo-p dir)
    (user-error "Not a git repo"))

  (-let* ((default-directory dir)
          (remote (magit-get-push-remote))
          (branch (magit-get-push-branch))
          (cmd (format "git config --get remote.%s.url" remote))
          (remote-url (s-trim (shell-command-to-string cmd)))
          ((_input _prot _user host _port filepath _suffix)
           (s-match (rx (? (group (+? nonl)) "://")
                        (? (group (+? nonl)) "@")
                        (group (+ (not (any "/" ":"))))
                        (? ":" (group (+ digit)) "/")
                        (? ":")
                        (group (* nonl))
                        (group ".git"))
                    remote-url))
          (branch (-last-item (s-split "/" branch)))
          (url
           (pcase host
             (`"bitbucket.org"
              (message "branch: %s" branch)
              (format "https://bitbucket.org/%s/branch/%s" filepath branch))
             (`"github.com"
              (format "https://github.com/%s/tree/%s" filepath branch))
             (`"gogs.movio.co"
              (format "https://gogs.movio.co/%s/src/%s" (s-chop-prefix "/" filepath) branch))
             (host
              (error "Don't know how to browse URL for host: %s" host)))))
    (browse-url url)))
