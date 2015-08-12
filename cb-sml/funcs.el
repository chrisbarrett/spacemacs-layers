;;; funcs.el --- Supporting functions for SML config  -*- lexical-binding: t; -*-

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

(require 'dash nil t)
(require 's nil t)

(defun cb-sml/at-datatype-decl? ()
  (or (s-matches? (rx bol (* space) (or "|" "datatype")) (current-line))
      (when (s-matches? "=" (current-line))
        (save-excursion
          (forward-line -1)
          (s-matches? (rx bol (* space) "datatype") (current-line))))))

(defun cb-sml/at-case-header? ()
  (s-matches? (rx "case" (+ nonl) "of" (* space) eol) (current-line)))

(defun cb-sml/at-case? ()
  (s-matches? (rx bol (* space) (or "of" "|") (+ nonl) "=>") (current-line)))

(defun cb-sml/at-val-binding? ()
  (s-matches? (rx bol (* space) "val" eow) (current-line)))

(defun cb-sml/at-fun-case? ()
  (-when-let (fun-name (cb-sml/maybe-fname-for-case-at-point))
    (s-matches? (rx-to-string `(and bol (* space) "|" (+ space) ,fun-name))
                (current-line))))

(defun cb-sml/at-fun-decl? ()
  (s-matches? (rx bol (* space) (or "and" "fun") eow) (current-line)))

(defun cb-sml/maybe-fname-for-case-at-point ()
  (save-excursion
    (goto-char (line-end-position))
    (when (search-backward-regexp (rx bol (* space) (or "and" "fun") (+ space)
                                      (group (+ (not space))))
                                  nil t)
      (match-string 1))))

(defun cb-sml/m-ret ()
  (interactive)
  (cond
   ((cb-sml/at-fun-case?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to col)
      (yas-expand-snippet (format "| %s ${1:bindings} = $0" (cb-sml/maybe-fname-for-case-at-point)))
      (message "Inserted function case.")))

   ((cb-sml/at-fun-decl?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to (+ col sml-indent-level))
      (yas-expand-snippet (format "| %s ${1:bindings} = $0" (cb-sml/maybe-fname-for-case-at-point)))
      (message "Inserted function case.")))

   ((cb-sml/at-case-header?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column (+ col sml-indent-level))
      (yas-expand-snippet "| ${1:binding} => $0")
      (message "Inserted pattern match case.")))

   ((cb-sml/at-case?)
    (let ((col (if (s-matches? (rx bol (* space) "of" eow) (current-line))
                   (1+ (current-indentation))
                 (current-indentation))))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "| ${1:binding} => $0")
      (message "Inserted pattern match case.")))

   ((cb-sml/at-datatype-decl?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (insert "| ")
      (message "Inserted case.")))

   ((and (cb-sml/at-val-binding?)
         (equal "sig" (f-ext (buffer-file-name))))
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "val ${1:name} : ${2:type}")
      (message "Inserted val declaration.")))

   ((cb-sml/at-val-binding?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "val ${1:name} = $0")
      (message "Inserted val declaration.")))

   (t
    (reindent-then-newline-and-indent)))

  (evil-insert-state))

(defun cb-sml/inf-sml-m-ret ()
  (interactive)
  (goto-char (point-max))
  (insert ";")
  (comint-send-input)
  (evil-insert-state))

;; snippet utils

(defun cb-sml/at-start-of-expr-for-snippet? (snippet-trigger-length)
  (let ((line-to-trigger (buffer-substring (line-beginning-position) (- (point) snippet-trigger-length))))
    (or (yas/bol?)
        (s-matches? (rx (or "=" "=>") (* space) eos) line-to-trigger))))

;;; funcs.el ends here
