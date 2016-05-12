;;; cb-org-clock-cascade.el --- Promote TODOs to NEXTs when clocking in.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((org "8.3.4") (dash "2.12.1"))

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

(require 'org)
(require 'org-clock)
(require 'dash)

(defun cb-org-clock-cascade--at-todo-parent? ()
  "Non-nil if at a task with any todo subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun cb-org-clock-cascade--at-task? ()
  "Non-nil if at a todo with no subtasks."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun cb-org-clock-cascade-clock-in-to-next-state (&optional _kw)
  "Move the task at point from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO."
  (unless (and (boundp 'org-capture-mode) org-capture-mode)
    (cond
     ((and (-contains? '("TODO") (org-get-todo-state))
           (cb-org-clock-cascade--at-task?))
      "NEXT")
     ((and (-contains? '("NEXT") (org-get-todo-state))
           (cb-org-clock-cascade--at-todo-parent?))
      "TODO"))))

;;;###autoload
(defun cb-org-clock-cascade-init ()
  (setq org-clock-in-switch-to-state #'cb-org-clock-cascade-clock-in-to-next-state))

(provide 'cb-org-clock-cascade)

;;; cb-org-clock-cascade.el ends here
