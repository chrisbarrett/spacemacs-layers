;;; cb-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((org "8.3.5") (dash "2.12.1"))

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
(require 'org)

(autoload 'org-agenda-filter-apply "org-agenda")

;;;###autoload
(defun cb-org-goto-diary ()
  "Switch to the diary file."
  (interactive)
  (find-file org-agenda-diary-file))

;;;###autoload
(defun cb-org-goto-notes ()
  "Switch to the default notes file."
  (interactive)
  (find-file org-default-notes-file))

;;;###autoload
(defun cb-org-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file cb-org-work-file))

;;;###autoload
(defun cb-org-goto-todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))

;;;###autoload
(defun cb-org-goto-tags-list ()
  "Show all tagged items."
  (interactive)
  (org-tags-view nil))

(defconst cb-org-goto--show-agenda-work-start-hour 8)
(defconst cb-org-goto--show-agenda-work-end-hour 17)

(defun cb-org-goto--is-work-time? (time)
  (-let* (((_s _m h d m y) time)
          (day-of-week (calendar-day-of-week (list m d y))))
    (and (<= cb-org-goto--show-agenda-work-start-hour h)
         (>= cb-org-goto--show-agenda-work-end-hour h)
         (<= 1 day-of-week)
         (>= 5 day-of-week))))

;;;###autoload
(defun cb-org-goto-agenda ()
  "Show the agenda fullscreen."
  (interactive)
  (let ((agenda-key (if (cb-org-goto--is-work-time? (decode-time)) "w" "A")))
    (org-agenda current-prefix-arg agenda-key))
  (delete-other-windows))

(provide 'cb-org-goto)

;;; cb-org-goto.el ends here
