;;; coq-unicode.el --- Unicode propertisation for Coq.  -*- lexical-binding: t; -*-

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

(autoload 'cb-font-lock-replace-match "funcs")

;;;###autoload
(defun coq-unicode-init ()
  (with-eval-after-load 'coq
    (dolist (mode '(coq-mode coq-response-mode coq-goals-mode))
      (font-lock-add-keywords
       mode
       (list
        (cb-font-lock-replace-match (rx (and (or bol (any "," ":" "(" "[" ">")) (* space)) bow (group "forall") eow) 1 (string-to-char "∀"))
        (cb-font-lock-replace-match (rx (and (or bol (any "," ":" "(" "[" ">")) (* space)) bow (group "exists") eow) 1 (string-to-char "∃"))
        (cb-font-lock-replace-match (rx (or space eow) (group "->")  (or space eol bow)) 1 (string-to-char "→"))
        (cb-font-lock-replace-match (rx (or space eow) (group "=>")  (or space eol bow)) 1 (string-to-char "⇒"))
        (cb-font-lock-replace-match (rx (or space eow) (group ">=")  (or space eol bow)) 1 (string-to-char "≥"))
        (cb-font-lock-replace-match (rx (or space eow) (group "<=")  (or space eol bow)) 1 (string-to-char "≤")))))))

(provide 'coq-unicode)

;;; coq-unicode.el ends here
