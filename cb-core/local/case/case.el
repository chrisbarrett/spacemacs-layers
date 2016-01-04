;;; case.el --- Commands for interactively transforming text case.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.9.0"))

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
(require 'thingatpt)

(defun case-transform (fn beg end)
  "Apply function FN to the text between BEG and END."
  (let ((updated (funcall fn (buffer-substring beg end))))
    (goto-char beg)
    (delete-region beg end)
    (insert updated)
    (goto-char beg)))

(defun case-symbol-bounds ()
  "Return a list of either the current region or current symbol bounds."
  (if (thing-at-point 'symbol)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)))
    (user-error "Point must be at a symbol")))

(defun upper-camel-case (beg end)
  (interactive (case-symbol-bounds))
  (case-transform 's-upper-camel-case beg end))

(defun lower-camel-case (beg end)
  (interactive (case-symbol-bounds))
  (case-transform 's-lower-camel-case beg end))

(defun snake-case (beg end)
  (interactive (case-symbol-bounds))
  (case-transform 's-snake-case beg end))

(defun screaming-snake-case (beg end)
  (interactive (case-symbol-bounds))
  (case-transform (lambda (s) (s-upcase (s-snake-case s))) beg end))

(defun dashed-words-case (beg end)
  (interactive (case-symbol-bounds))
  (case-transform 's-dashed-words beg end))

(autoload 'evil-ex-define-cmd "evil-ex")

(with-eval-after-load 'evil
  (evil-ex-define-cmd "ucc" 'upper-camel-case)
  (evil-ex-define-cmd "lcc" 'lower-camel-case)
  (evil-ex-define-cmd "sc" 'snake-case)
  (evil-ex-define-cmd "ssc" 'screaming-snake-case)
  (evil-ex-define-cmd "dwc" 'dashed-words-case)
  )

(provide 'case)

;;; case.el ends here
