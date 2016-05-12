;;; cb-org-recalculate-whole-table.el --- Recalculate table with C-c C-c.  -*- lexical-binding: t; -*-

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

;; Recalculate the table at point with C-c C-c.

;;; Code:

;;;###autoload
(defun cb-org-recalculate-whole-table ()
  "Recalculate the current table using `org-table-recalculate'."
  (interactive "*")
  (when (org-at-table-p)
    (let ((before (buffer-substring (org-table-begin) (org-table-end))))
      (org-table-recalculate '(16))
      (let ((after (buffer-substring (org-table-begin) (org-table-end))))
        (if (equal before after)
            (message "Table up-to-date")
          (message "Table updated"))))))

;;;###autoload
(defun cb-org-recalculate-whole-table-init ()
  (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-recalculate-whole-table))

(provide 'cb-org-recalculate-whole-table)

;;; cb-org-recalculate-whole-table.el ends here
