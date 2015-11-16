;;; cb-bootstrap.el --- File for bootstrapping layers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; This file contains lisp needed for bootstrapping my configuration. It's
;; extracted from my .spacemacs so people who want to live dangerously and use
;; my config can easily do so with minimal changes to their .spacemacs.

;;; Code:

(autoload 'package-installed-p "package")

(defvar cb-bootstrap/package-installation-attempts 2)

(defun cb-bootstrap/user-init ()
  ;; Show a backtrace if I've stuffed up something in my configuration.
  (setq debug-on-error t)
  (setq debug-on-quit t)

  ;; The org repo is required for `org-plus-contrib'. This means `package.el'
  ;; must be explicitly (re)initialised.
  (require 'package)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (unless package-alist (package-refresh-contents))
  (package-initialize)

  ;; The following packages are required by layers at the top-level, and must be
  ;; manually installed before Spacemacs loads those layers.
  (cb-bootstrap--install-package 's)
  (cb-bootstrap--install-package 'noflet)
  (cb-bootstrap--install-package 'f)
  (cb-bootstrap--install-package 'let-alist)
  (cb-bootstrap--install-package 'dash)
  (cb-bootstrap--install-package 'dash-functional)
  (cb-bootstrap--install-package 'helm) ;; HACK: needed for Spacemacs

  ;; Some random utilities and editor tools are installed in these dirs.
  (add-to-list 'exec-path "~/.cabal/bin/")
  (add-to-list 'exec-path "~/bin/")

  ;; Ensure the `cb-core' layer is loaded before all others. This layer contains
  ;; utilities needed by other layers.
  (load (concat user-layers-directory "cb-core/funcs.el"))
  (load (concat user-layers-directory "cb-core/config.el")))

(defun cb-bootstrap/user-config ()

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (with-demoted-errors "Personal config: %S"
    (require 'personal-config nil t))

  ;; Disable bookmarks.
  (setq bookmark-save-flag nil)

  ;; Disable debugging now that my configuration has loaded.
  (setq debug-on-error nil)
  (setq debug-on-quit nil))

(defun cb-bootstrap--mk-package-dir-regexp (pkg)
  (rx-to-string `(and ,(symbol-name pkg)
                      "-" (repeat 8 digit) "." (repeat 3 4 digit) (? "/"))))

(defun cb-bootstrap--install-package (pkg &optional attempts cur)
  (cond
   ((null attempts)
    (cb-bootstrap--install-package pkg cb-bootstrap/package-installation-attempts 1))
   ((< attempts cur)
    (error "Unable to install %s after %s attempt(s)" pkg attempts))
   (t
    (if (equal 1 cur)
        (message "--> Installing package %s..." pkg)
      (message "--> Installing package %s... (attempt %s/%s)" pkg cur attempts))
    (condition-case err
        (cond
         ((require 'paradox nil t)
          (paradox-require pkg))
         ((package-installed-p pkg)
          (require pkg))
         (t
          (package-install pkg)
          (require pkg)))
      (error
       (let ((archives (concat package-user-dir "/archives")))
         (when (file-directory-p archives)
           (message "--> Cleaning package archives...")
           (delete-directory archives t)))

       (dolist (entry (directory-files package-user-dir t))
         (when (string-match-p (cb-bootstrap--mk-package-dir-regexp pkg) (file-name-nondirectory entry))
           (message "--> Deleting existing package at %s..." entry)
           (delete-directory entry t)))

       (package-refresh-contents)
       (package-initialize)
       (cb-bootstrap--install-package pkg attempts (1+ cur)))))))

(provide 'cb-bootstrap)

;;; cb-bootstrap.el ends here
