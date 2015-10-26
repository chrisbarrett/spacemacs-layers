;;; funcs.el --- Functions for OCaml layer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

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

(require 's nil t)
(require 'dash nil t)
(require 'yasnippet nil t)
(require 'f nil t)

(defun cb-ocaml/at-match-header? ()
  (s-matches? (rx "match" (+ nonl) "with" (* space) eol) (current-line)))

(defun cb-ocaml/at-case? ()
  (s-matches? (rx bol (* space) (or "with" "|") (+ nonl) "->") (current-line)))

(defun cb-ocaml/at-type-decl? ()
  (or (s-matches? (rx bol (* space) (or "|" "type")) (current-line))
      (when (s-matches? "=" (current-line))
        (save-excursion
          (forward-line -1)
          (s-matches? (rx bol (* space) "type") (current-line))))))

(defun cb-ocaml/at-let-binding? ()
  (s-matches? (rx bol (* space) "let" eow) (current-line)))

(defun cb-ocaml/at-import? ()
  (s-matches? (rx bol (* space) "open" eow) (current-line)))

(defun cb-ocaml/at-function-keyword? ()
  (s-matches? (rx bow "function" (* space) eol) (current-line)))

(defun cb-ocaml/m-ret ()
  (interactive)
  (cond
   ((cb-ocaml/at-import?)
    (goto-char (line-end-position))
    (newline)
    (insert "open "))

   ((or (cb-ocaml/at-match-header?)
        (cb-ocaml/at-function-keyword?))
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column (+ col tuareg-match-indent))
      (yas-expand-snippet "| ${1:binding} -> $0")
      (message "Inserted pattern match case.")))

   ((cb-ocaml/at-case?)
    (let ((col (if (s-matches? (rx bol (* space) "with" eow) (current-line))
                   (1+ (current-indentation))
                 (current-indentation))))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "| ${1:binding} -> $0")
      (message "Inserted pattern match case.")))

   ((cb-ocaml/at-type-decl?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (insert "| ")
      (message "Inserted case.")))

   ((and (cb-ocaml/at-let-binding?)
         (equal "sig" (f-ext (buffer-file-name))))
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "let ${1:name} : ${2:type}")
      (message "Inserted val declaration.")))

   ((cb-ocaml/at-let-binding?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "let ${1:name} = $0")
      (message "Inserted val declaration.")))

   (t
    (reindent-then-newline-and-indent)))

  (evil-insert-state))
