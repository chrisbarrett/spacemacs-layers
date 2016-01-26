;;; cb-project-show-project.el --- Display a list of options when switching projects. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((f "0.17.2") (hydra "0.13.4") (noflet "0.0.15") (projectile "0.13.0") (helm-projectile-ag "0.13.0") (magit "2.1.0"))

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

(require 'f)
(require 'hydra)
(require 'noflet)
(require 'projectile)

(autoload 'helm-projectile-ag "helm-projectile")
(autoload 'magit-status "magit")
(autoload 'sbt-start "sbt-mode")


;;; HACK: Define a macro to rebind `default-directory' for hydra commands. This
;;; is needed for those commands to operate in the project being switched to.

(defvar cb-project-show-project--project-to-switch)

(defun cb-project-show-project--set-project-to-switch ()
  (setq cb-project-show-project--project-to-switch default-directory))

(defmacro cb-project-show-project--with-project-as-default-directory (&rest body)
  "Execute BODY forms with `default-directory' bound to the project root.

Also binds the variable `it' to that directory."
  (declare (indent 0))
  `(let* ((default-directory cb-project-show-project--project-to-switch)
          (it default-directory))
     ,@body))



(defun cb-project-show-project--sbt-for-dir (dir)
  (noflet ((sbt:find-root (&rest args) (f-abbrev dir)))
    (let ((buf (save-window-excursion (sbt-start))))
      (pop-to-buffer buf))))

(defhydra cb-project-show-project (:color blue :foreign-keys warn)
  "Execute in project"
  ("/" (cb-project-show-project--with-project-as-default-directory (helm-projectile-ag)) "ag")
  ("d" (cb-project-show-project--with-project-as-default-directory (dired it)) "dired")
  ("f" (cb-project-show-project--with-project-as-default-directory (projectile-find-file)) "find file")
  ("g" (cb-project-show-project--with-project-as-default-directory (magit-status it)) "magit status")
  ("S" (cb-project-show-project--with-project-as-default-directory (cb-project-show-project--sbt-for-dir it)) "SBT")
  ("q" nil "cancel"))

(defun cb-project-show-project-init ()
  (add-hook 'projectile-before-switch-project-hook #'cb-project-show-project--set-project-to-switch)
  (setq projectile-switch-project-action #'cb-project-show-project/body))

(provide 'cb-project-show-project)

;;; cb-project-show-project.el ends here
