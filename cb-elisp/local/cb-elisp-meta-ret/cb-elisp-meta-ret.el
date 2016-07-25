;;; cb-elisp-meta-ret.el --- <enter description here>  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 'thingatpt)

(autoload 'cb-macros-until "cb-macros")
(autoload 'evil-insert-state "evil-state")
(autoload 'sp-backward-up-sexp "smartparens")
(autoload 'sp-up-sexp "smartparens")

(defconst cb-elisp-meta-ret--let-expression-re
  (regexp-opt '("(let" "(-if-let*" "(-when-let*" "(-let"))
  "Regex matching the start of a let expression.")

(defun cb-elisp-meta-ret-:let-expr-start ()
  "Move to the start of a let expression."
  (cl-flet ((at-let? () (thing-at-point-looking-at cb-elisp-meta-ret--let-expression-re)))
    (while (and (sp-backward-up-sexp) (not (at-let?))))
    (when (at-let?) (point))))

(defun cb-elisp-meta-ret--at-let-binding-form? ()
  "Non-nil if point is at the top of a binding form in a let expression."
  (and (save-excursion (cb-elisp-meta-ret-:let-expr-start))
       (save-excursion
         (sp-backward-up-sexp 3)
         (thing-at-point-looking-at cb-elisp-meta-ret--let-expression-re))))

;;;###autoload
(defun cb-elisp-meta-ret ()
  "Perform context-sensitive newline behaviour."
  (interactive)
  (cond
   ;; Insert let-binding
   ((save-excursion (cb-elisp-meta-ret-:let-expr-start))
    (cb-macros-until (cb-elisp-meta-ret--at-let-binding-form?) (sp-backward-up-sexp))
    (sp-up-sexp)
    (newline-and-indent)
    (insert "()")
    (forward-char -1))
   (t
    (sp-up-sexp)
    (newline-and-indent)
    (when (and (boundp 'evil-mode) evil-mode)
      (evil-insert-state)))))

(provide 'cb-elisp-meta-ret)

;;; cb-elisp-meta-ret.el ends here
