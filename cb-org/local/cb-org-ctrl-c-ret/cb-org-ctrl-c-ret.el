;;; cb-org-ctrl-c-ret.el --- Context-sensitive Ctrl-C RET keybinding.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

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

;;; Code:

(autoload 'org-at-table-p "org")
(autoload 'org-insert-todo-heading "org")
(autoload 'org-table-hline-and-move "org-table")

(defun cb-org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading'."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-hline-and-move))
   (t
    (call-interactively #'org-insert-todo-heading))))

(provide 'cb-org-ctrl-c-ret)

;;; cb-org-ctrl-c-ret.el ends here
