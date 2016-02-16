;;; elm-meta-ret.el --- Smart M-RET command for Elm.  -*- lexical-binding: t; -*-

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

(require 'elm-mode)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-insert-state "evil-states")
(autoload 'yas-exit-all-snippets "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")

(defun elm-meta-ret--newline-indent-to-same-col ()
  "Make a new line below the current one and indent to the same column."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

(defun elm-meta-ret--at-decl-for-function? (fname)
  (when fname
    (or
     ;; A type decl exists in this buffer?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ space) ":"))
                 (buffer-string))
     ;; At an equation?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ nonl) "="))
                 (cb-buffers-current-line)))))

(defun elm-meta-ret--first-ident-on-line ()
  (car (-difference (s-split (rx space) (cb-buffers-current-line) t)
                    elm--keywords)))

(defun elm-meta-ret--at-incomplete-type-decl-header? ()
  (let ((from-prev-line (save-excursion
                          (forward-line -1)
                          (buffer-substring (line-beginning-position) (point-max))))
        (decl-rx (rx bol (* space) "type" (+ space) (+ alnum))))
    (and (s-matches? decl-rx from-prev-line)
         (not (elm-meta-ret--after-complete-type-decl-header?)))))

(defun elm-meta-ret--after-complete-type-decl-header? ()
  (let ((from-prev-line (save-excursion
                          (forward-line -1)
                          (buffer-substring (line-beginning-position) (point-max))))
        (decl-rx (rx bol (* space) "type" (+ space) (+ alnum) (? eol) (* space) "=")))
    (s-matches? decl-rx from-prev-line)))

;;;###autoload
(defun elm-meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Insert new import
   ((s-matches? (rx bol "import") (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (insert "import ")
    (message "New import"))

   ;; Insert new case below the current type decl.
   ((elm-meta-ret--after-complete-type-decl-header?)
    (goto-char (line-end-position))
    (newline)
    (insert "| ")
    (goto-char (line-beginning-position))
    (elm-indent-cycle)
    (goto-char (line-end-position))
    (message "New data case"))

   ;; Insert '=' for incomplete type decls.
   ((elm-meta-ret--at-incomplete-type-decl-header?)
    (goto-char (line-end-position))
    (newline)
    (insert "= ")
    (goto-char (line-beginning-position))
    (elm-indent-cycle)
    (goto-char (line-end-position))
    (message "Completing type decl"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "|") (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (insert "| ")
    (message "New data case"))

   ;; New function case.
   ((elm-meta-ret--at-decl-for-function? (elm-meta-ret--first-ident-on-line))
    (let ((fname (elm-meta-ret--first-ident-on-line)))
      (elm-meta-ret--newline-indent-to-same-col)
      (insert fname)
      (just-one-space)
      (message "New binding case")))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (* space) ",") (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (* space) "->") (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (insert (format "-> "))
    (message "New arrow"))

   ;; Insert new pattern match case below the current one.
   ((s-matches? (rx bol (* space) (+ (not (any "="))) "->") (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (yas-expand-snippet "${1:pat} -> $0")
    (message "New pattern match case"))
   ((s-matches? (rx bol (* space) "case" (+ space)) (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (yas-expand-snippet "  ${1:pat} -> $0")
    (message "New pattern match case"))

   ;; Insert new line starting with a comma for the current braced expr
   ((s-matches? (rx bol (* space) (or "[" "{")) (cb-buffers-current-line))
    (elm-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)
    (message "New line")))

  (evil-insert-state))

;;;###autoload
(defun elm-meta-ret-init ()
  (define-key elm-mode-map (kbd "M-RET") #'elm-meta-ret))

(provide 'elm-meta-ret)

;;; elm-meta-ret.el ends here
