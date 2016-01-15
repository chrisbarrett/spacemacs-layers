;;; cb-bootstrap.el --- File for bootstrapping layers.  -*- mode: emacs-lisp; lexical-binding: t; -*-

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

(require 'package)
(autoload 'paradox-require "paradox")

(defconst cb-bootstrap-packages
  '(s
    dash
    dash-functional
    noflet
    f
    let-alist
    hydra
    )
  "Packages required for bootstrapping my configuration.")

(defconst cb-bootstrap-additional-exec-path-entries
  '("~/.cabal/bin/"
    "~/bin")
  "Additional paths to add to `exec-path'.  They may contain utilities needed for bootstrap.")

(defconst cb-bootstrap-package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/"))
  "An alist of package archives required during bootstrap.")

(defconst cb-bootstrap-preload-lisp-files
  (list
   (concat user-layers-directory "cb-vars.el")
   (concat user-layers-directory "cb-core/funcs.el")
   (concat user-layers-directory "cb-core/config.el"))
  "Aggressively load these packages.  They contain utilities needed in layer definitions.")

(defvar cb-bootstrap/package-installation-attempts 2
  "Abort package installation after this number of failed attempts.")

(defun cb-bootstrap/enable-debugging ()
  "Show a backtrace if I've stuffed up something in my configuration."
  (setq debug-on-error t)
  (setq debug-on-quit t))

(defun cb-bootstrap/initialize-packages ()
  (dolist (archive cb-bootstrap-package-archives)
    (add-to-list 'package-archives archive))
  (unless (file-exists-p (concat user-emacs-directory "elpa"))
    (package-refresh-contents))
  (package-initialize)
  (dolist (pkg cb-bootstrap-packages)
    (cb-bootstrap--install-package pkg)))

(defun cb-bootstrap/load-preloadable-lisp-files ()
  (dolist (el cb-bootstrap-preload-lisp-files)
    (load el)))

(defun cb-bootstrap/initialize-exec-path ()
  (dolist (dir cb-bootstrap-additional-exec-path-entries)
    (add-to-list 'exec-path dir)))

(defun cb-bootstrap/user-config ()
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  (with-demoted-errors "Personal config: %S"
    (require 'personal-config nil t)))

(defun cb-bootstrap/disable-debugging ()
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
