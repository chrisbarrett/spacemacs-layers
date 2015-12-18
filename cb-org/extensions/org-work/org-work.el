;;; org-work.el --- Configure orgmode for use as a work management suite.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((f "0.17") (s "1.9") (dash "2.8") (emacs "24.3"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Configure orgmode for use as a work management suite.

;;; Code:

;; HACK: Fix flycheck load path for Cask.
(eval-and-compile
  (dolist (x (file-expand-wildcards "./.cask/**/**/**"))
    (add-to-list 'load-path x)))

(require 'org)
(require 'org-agenda)
(require 's)
(require 'f)
(require 'dash)

;;; Customisable vars


(defvar org-work-file (f-join org-directory "work.org")
  "Defines the path to file for work-related todos, etc.")

(defvar org-work-persistence-file (f-join org-directory ".org-at-work")
  "File to create that saves work state between sessions.")

;;; Clocking

(defvar org-work--at-work? nil
  "Non-nil if currently 'at work'.")

(defvar org-work-started-work-hook nil
  "Hook run after starting work.")

(defvar org-work-left-work-hook nil
  "Hook run after leaving work.")

(defvar org-work-state-changed-hook nil
  "Hook run after starting or leaving work.")

(defun org-work--goto-or-create-work-log ()
  "Move to today's date log entry, creating it if necessary."
  (-let [(_s _m _h d mo y &rest _) (decode-time (current-time))]
    (org-datetree-find-date-create (list mo d y))))

(defun org-work-start-work (file)
  "Start work, with FILE as the project file."
  (with-current-buffer (find-file-noselect file)
    (org-work--goto-or-create-work-log)
    (org-clock-in))
  (setq org-work--at-work? t)
  (f-touch org-work-persistence-file)
  (run-hooks 'org-work-started-work-hook)
  (message "Started work"))

(defun org-work-leave-work (file)
  "Leave work, with FILE as the project file."
  (with-current-buffer (find-file-noselect file)
    (org-work--goto-or-create-work-log)
    (org-clock-out nil t))
  (setq org-work--at-work? nil)
  (when (f-exists? org-work-persistence-file)
    (f-delete org-work-persistence-file))

  (run-hooks 'org-work-left-work-hook)
  (message "Left work"))

(defun org-work-toggle-at-work (file)
  "Toggle whether I am currently at work.  FILE is the project file."
  (interactive (list org-work-file))
  (if org-work--at-work?
      (org-work-leave-work file)
    (org-work-start-work file))
  (run-hooks 'org-work-state-changed-hook))

(defun org-work-maybe-start-work ()
  "Set status to 'at-work' if the work persistence file exists."
  (when (f-exists? org-work-persistence-file)
    (org-work-start-work org-work-file)))

;;;###autoload
(define-minor-mode org-work-mode
  "Minor mode for work-related org buffers.")

(defun maybe-enable-org-work-mode ()
  "Enable org-work-mode for ORG-WORK org buffers."
  (when (s-matches? (rx "work" (* anything) "org" eol)
                    (buffer-name))
    (org-work-mode +1)))

(provide 'org-work)

;;; org-work.el ends here
