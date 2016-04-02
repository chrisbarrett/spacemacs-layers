;;; insert-variable-value.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((dash "2.12.1"))

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

(defun insert-variable-value--filter-atoms (predicate)
  "Return the elements of the default obarray that match PREDICATE."
  (let (acc)
    (mapatoms (lambda (atom)
                (when (funcall predicate atom)
                  (push atom acc))))
    acc))

(defun insert-variable-value--read-variable ()
  (let ((symbols (-map #'symbol-name
                       (insert-variable-value--filter-atoms
                        (lambda (it)
                          (or (custom-variable-p it)
                              (special-variable-p it)))))))
    (intern (completing-read "Variable: " symbols))))

;;;###autoload
(defun insert-variable-value (variable)
  "Insert the value of VARIABLE at point."
  (interactive (list (insert-variable-value--read-variable)))
  (insert (pp-to-string (eval variable))))

(provide 'insert-variable-value)

;;; insert-variable-value.el ends here
