;;; cb-emmet-tolerant-expansion.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((dash "2.12.1") (yasnippet "20160713.1403") (smartparens "20160706.649") (emmet-mode "20160501.1151"))

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

(require 'cl-lib)
(require 'dash)

(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'sp-up-sexp "smartparens")
(autoload 'yas--templates-for-key-at-point "yasnippet")
(autoload 'yas-expand "yasnippet")

(defun cb-emmet-tolerant-expansion--emmet-move-out-of-squares ()
  "Move point outside squares before expansion."
  (cl-flet ((sp-has-op? (op)
                        (-when-let ((&plist :op o) (sp-get-enclosing-sexp))
                          (equal op o)))
            (move-out-of-sexp ()
                              (-when-let ((&plist :end end) (sp-get-enclosing-sexp))
                                (goto-char end))))
    (cond
     ((sp-has-op? "[")
      (move-out-of-sexp))
     ((and (sp-has-op? "\"")
           (save-excursion
             (sp-up-sexp)
             (sp-has-op? "[")))
      (move-out-of-sexp)
      (move-out-of-sexp)))))

(defun cb-emmet-tolerant-expansion--expand-snippet-then-emmet (f &rest args)
  (if (yas--templates-for-key-at-point)
      (call-interactively #'yas-expand)
    (cb-emmet-tolerant-expansion--emmet-move-out-of-squares)
    (apply f args)))

;;;###autoload
(defun cb-emmet-tolerant-expansion-init ()
  (advice-add 'emmet-expand-yas :around #'cb-emmet-tolerant-expansion--expand-snippet-then-emmet))

(provide 'cb-emmet-tolerant-expansion)

;;; cb-emmet-tolerant-expansion.el ends here
