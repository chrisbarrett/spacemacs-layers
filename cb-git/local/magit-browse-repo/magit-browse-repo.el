;;; magit-browse-repo.el --- Command to browse online page corresponing to a git repo.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (dash "2.12.1") (magit "20160320.152") (projectile "0.13.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'magit)
(require 's)

(autoload 'projectile-project-p "projectile")
(autoload 'projectile-completing-read "projectile")
(autoload 'projectile-relevant-known-projects "projectile")

;;;###autoload
(defun magit-browse-repo (dir)
  "Open project page for the repo at DIR."
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

(provide 'magit-browse-repo)

;;; magit-browse-repo.el ends here
