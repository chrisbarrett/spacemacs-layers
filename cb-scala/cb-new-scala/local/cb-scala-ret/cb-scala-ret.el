;;; cb-scala-ret.el --- Context-sensitive newline commands.  -*- lexical-binding: t; -*-

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

(require 'cb-buffers)
(require 'dash)
(require 's)
(require 'sp-generic-prog)

(autoload 'evil-insert-state "evil-states")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'yas-expand-snippet "yasnippet")

(defun cb-scala-ret--brace-group-starts-with-case-expr? ()
  (when (sp-inside-curly-braces-with-content?)
    (-let [(&plist :beg beg :end end) (sp-get-enclosing-sexp)]
      (s-matches? (rx (* space) "case")
                  (buffer-substring (1+ beg) (1- end))))))

(defun cb-scala-ret--maybe-swing-down-lambda-body ()
  (when (search-backward-regexp (rx (or "=>" "⇒")) (line-beginning-position) t)
    (goto-char (match-end 0))
    (newline-and-indent)))

(defun cb-scala-ret--after-lambda-arrow? ()
  (s-matches? (rx (* space) (or "=>" "⇒") (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun cb-scala-ret--blank-up-to-curly? ()
  (s-matches? (rx bos (* space) "}") (buffer-substring (point) (line-end-position))))

(defun cb-scala-ret--at-scaladoc? ()
  (s-matches? (rx bol (* space) (? "/") (+ "*")) (cb-buffers-current-line)))

(defun cb-scala-ret--at-case-class? ()
  (s-matches? (rx bol (* space) "case" (+ space) "class" eow) (cb-buffers-current-line)))

(defun cb-scala-ret--at-case-object? ()
  (s-matches? (rx bol (* space) "case" (+ space) "object" eow) (cb-buffers-current-line)))

(defun cb-scala-ret--at-abstract-sealed-class? ()
  (s-matches? (rx bol (* space) "abstract" (+ space) "sealed" (+ space) "class" eow) (cb-buffers-current-line)))

(defun cb-scala-ret--at-sealed-trait? ()
  (s-matches? (rx bol (* space) "sealed" (+ space) "trait" eow) (cb-buffers-current-line)))

(defun cb-scala-ret--open-line-below-current-indentation ()
  "Open a new line below at the current indent level."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

;;;###autoload
(defun cb-scala-ret (&optional arg)
  "Insert a newline with context-sensitive formatting.
With ARG, just insert a newline."
  (interactive "P")
  (let ((sexp (sp-get-enclosing-sexp)))
    (cond
     (arg
      (comment-indent-new-line)
      (just-one-space))

     ((cb-scala-ret--at-scaladoc?)
      (goto-char (line-end-position))
      (cb-scala-ret--open-line-below-current-indentation)
      (insert "* "))

     ((cb-buffers-in-string-or-comment?)
      (comment-indent-new-line)
      (just-one-space))

     ((sp-inside-curly-braces-blank-content? nil sexp)
      (sp-split-braced-expression-over-new-lines (rx ";") sexp))

     ((and (sp-inside-curly-braces-with-content? t sexp)
           (cb-scala-ret--blank-up-to-curly?)
           (cb-scala-ret--after-lambda-arrow?))
      (sp-split-braced-expression-over-new-lines (rx ";") sexp)
      (goto-char (line-end-position))
      (save-excursion
        (cb-scala-ret--maybe-swing-down-lambda-body))
      (newline-and-indent))

     ((and (sp-inside-curly-braces-with-content? t sexp)
           (cb-scala-ret--brace-group-starts-with-case-expr?))
      (sp-split-braced-expression-over-new-lines (rx ";") sexp)
      (cond
       ((cb-scala-ret--after-lambda-arrow?)
        (newline-and-indent))
       (t
        (goto-char (line-end-position))
        (cb-scala-ret--maybe-swing-down-lambda-body)))

      (goto-char (line-end-position)))

     (t
      (sp-generic-prog-ret)))))

;;;###autoload
(defun cb-scala-meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- In match statements
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "var" eow) (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "var ${1:ident} = $0")
    (message "New var binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "private" (+ space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "private val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "protected" (+ space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "protected val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new case class.
   ((or (cb-scala-ret--at-case-class?) (cb-scala-ret--at-sealed-trait?) (cb-scala-ret--at-abstract-sealed-class?))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "case class ${1:Case}(${2:params...})")
    (message "New case class"))

   ;; Insert new case object.
   ((cb-scala-ret--at-case-object?)
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "case object ${1:Name}")
    (message "New case object"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "case") (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (yas-expand-snippet "case ${1:binding} => $0")
    (message "New data case"))

   ;; Insert new import statement
   ((s-matches? (rx bol (* space) "import" eow) (cb-buffers-current-line))
    (cb-scala-ret--open-line-below-current-indentation)
    (insert "import ")
    (message "New import statement"))

   (t
    (goto-char (line-end-position))
    (cb-scala-ret)))

  (evil-insert-state))

(provide 'cb-scala-ret)

;;; cb-scala-ret.el ends here
