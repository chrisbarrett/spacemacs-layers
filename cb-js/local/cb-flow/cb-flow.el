;;; cb-flow.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (dash "2.12.1"))

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
(require 'json)
(require 's)

(defun cb-flow-type-at (file line col)
  "Return the inferred type for FILE at the given LINE and COL.

When called interactively, print the type at point.
Otherwise return the parsed JSON response."
  (interactive
   (list
    (buffer-file-name)
    (line-number-at-pos)
    (current-column)))
  (let ((buffer (with-current-buffer (get-buffer-create " flow-type-at")
                  (erase-buffer)
                  (current-buffer))))

    (call-process "flow" nil buffer t
                  "type-at-pos"
                  file
                  (number-to-string line)
                  (number-to-string col)
                  "--json")
    (with-current-buffer buffer
      (-let [(parsed &as &alist 'type type) (json-read-from-string (buffer-string))]
        (if (called-interactively-p nil)
            (message "%s" type)
          parsed)))))

(defun cb-flow-insert-flow-annotation ()
  "Insert a flow annotation at the start of this file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (s-matches? (rx (or (and "//" (* space) "@flow")
                            (and "/*" (* space) "@flow" (* space) "*/")))
                    (buffer-substring (line-beginning-position) (line-end-position)))
        (user-error "Buffer already contains an @flow annotation")
      (insert "// @flow\n")
      (message "Inserted @flow annotation."))))

(provide 'cb-flow)

;;; cb-flow.el ends here
