;;; cb-org-ctrl-c-ctrl-k.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0"))

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

(require 's)

(autoload 'org-capture-kill "org-capture")
(autoload 'org-cut-subtree "org")
(autoload 'org-kill-note-or-show-branches "org")

(defun cb-org-ctrl-c-ctrl-k (&optional n)
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive "p")
  (cond
   ((and (boundp 'org-capture-mode) org-capture-mode)
    (org-capture-kill))
   ((s-starts-with? "*Org" (buffer-name))
    (org-kill-note-or-show-branches))
   (t
    (org-cut-subtree n))))

(provide 'cb-org-ctrl-c-ctrl-k)

;;; cb-org-ctrl-c-ctrl-k.el ends here
