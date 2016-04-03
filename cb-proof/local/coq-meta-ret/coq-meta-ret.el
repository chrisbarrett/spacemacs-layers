;;; coq-meta-ret.el --- Context-sensitive newline command for Coq.  -*- lexical-binding: t; -*-

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

(require 's)

(autoload 'evil-insert-state "evil-states")
(autoload 'yas-exit-all-snippets "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")

(defun coq-meta-ret--current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun coq-meta-ret--case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun coq-meta-ret--newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (if (search-forward "." (line-end-position) t)
      (forward-char -1)
    (goto-char (line-end-position)))

  (newline)
  (indent-to col)
  (insert str))

(defun coq-meta-ret--newline-and-indent-to-col (col)
  (goto-char (line-end-position))
  (newline)
  (indent-to col))

(defun coq-meta-ret--rx-start-column (rx)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp rx (line-beginning-position))
    (current-column)))

;;;###autoload
(defun coq-meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond

   ;; Insert case after match statement.

   ((s-matches? (rx symbol-start "match" symbol-end) (coq-meta-ret--current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (goto-char (max (line-beginning-position) (- (point) 2)))
    (yas-expand-snippet "| ${1:case} => $0"))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) (? "Co") "Inductive") (coq-meta-ret--current-line))
    (coq-meta-ret--newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert dependent type below the current one.
   ((s-matches? (rx bol (* space) "|" (* space) (+ word) (+ space) ":" (+ space))
                (coq-meta-ret--current-line))
    (coq-meta-ret--newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "=>") (coq-meta-ret--current-line))
    (coq-meta-ret--newline-and-indent-to-col (coq-meta-ret--case-start-col))
    (yas-expand-snippet "| ${1:case} => $0"))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (coq-meta-ret--current-line))
    (coq-meta-ret--newline-and-insert-at-col (coq-meta-ret--case-start-col) "| "))

   ;; Insert check.
   ((s-matches? (rx bol "Check") (coq-meta-ret--current-line))

    (coq-meta-ret--newline-and-indent-to-col (coq-meta-ret--rx-start-column "Check"))
    (yas-expand-snippet "Check $0."))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (evil-insert-state))

(defun coq-meta-ret-init ()
  (with-eval-after-load 'coq
    (with-no-warnings
      (define-key coq-mode-map (kbd "M-RET") #'coq-meta-ret))))

(provide 'coq-meta-ret)

;;; coq-meta-ret.el ends here
