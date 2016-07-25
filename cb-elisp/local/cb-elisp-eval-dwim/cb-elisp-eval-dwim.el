;;; cb-elisp-eval-dwim.el --- <enter description here>  -*- lexical-binding: t; -*-

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

(defun cb-elisp-eval-dwim--thing-for-eval ()
  (cond
   ((region-active-p)
    (call-interactively 'eval-region)
    (list :beg (region-beginning)
          :end (region-end)))

   ((thing-at-point 'defun)
    (save-excursion
      (beginning-of-defun)
      (call-interactively 'eval-defun)
      (list :beg (point)
            :end (save-excursion (end-of-defun) (point)))))

   ((ignore-errors (preceding-sexp))
    (call-interactively 'eval-last-sexp)
    (-when-let ((beg . end) (save-excursion
                              (backward-char)
                              (bounds-of-thing-at-point 'sexp)))
      (list :beg beg :end end)))))

(defun cb-elisp-eval-dwim ()
  "Perform a context-sensitive eval command.
Return the bounds of the evaluated form."
  (interactive)
  (-if-let ((&plist :beg beg :end end) (cb-elisp-eval-dwim--thing-for-eval))
      (eval-region beg end)
    (user-error "Nothing to evaluate")))

(provide 'cb-elisp-eval-dwim)

;;; cb-elisp-eval-dwim.el ends here
