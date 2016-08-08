;;; cb-haskell-meta-ret.el --- Smart newline command for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0") (dash "2.12.1") (haskell-mode "16.1-git") (smartparens "20160721.1448"))

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
(require 'thingatpt)

(autoload 'evil-forward-word-begin "evil-commands")
(autoload 'evil-insert-state "evil-states")
(autoload 'haskell-indentation-indent-line "haskell-indentation")
(autoload 'haskell-indentation-newline-and-indent "haskell-indentation")
(autoload 'shm/forward-node "shm-nav")
(autoload 'shm/goto-parent-end "shm-nav")
(autoload 'shm/reparse "shm-ast")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'sp-inside-curly-braces-blank-content? "sp-generic-prog")
(autoload 'sp-inside-curly-braces? "sp-generic-prog")
(autoload 'yas-exit-all-snippets "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")

(defconst cb-haskell-meta-ret--haskell-keywords
  '("let" "where" "module" "case" "class" "data" "deriving" "default"
    "import" "infixl" "infixr" "newtype" "data" "type" "if" "then" "else"
    "pattern"))

(defun cb-haskell-meta-ret--newline-indent-to-same-col ()
  "Make a new line below the current one and indent to the same column."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (when (s-ends-with? ".lhs" (buffer-name))
      (insert "> "))
    (indent-to col)))

(defun cb-haskell-meta-ret--first-ident-on-line ()
  (car (-difference (s-split (rx (? ">") space) (cb-buffers-current-line) t)
                    cb-haskell-meta-ret--haskell-keywords)))

(defun cb-haskell-meta-ret--line-content-relative (move-n-lines)
  "Return the line at point, or another line relative to this line.
MOVE-N-LINES is an integer that will return a line forward if
positive or backward if negative."
  (save-excursion
    (forward-line move-n-lines)
    (cb-buffers-current-line)))

(defun cb-haskell-meta-ret--in-data-decl? ()
  (cond
   ((cb-buffers-in-string-or-comment?) nil)
   ((s-matches? "}" (buffer-substring (line-beginning-position) (point))) nil)
   ((thing-at-point-looking-at (rx bol (? ">") (* space) "data ")) t)
   (t
    (save-excursion
      (when (search-backward-regexp (rx bol (? ">") (not space)) nil t)
        (thing-at-point-looking-at "data "))))))

(defun cb-haskell-meta-ret--at-decl-for-function? (fname)
  (when fname
    (or
     ;; A type decl exists in this buffer?
     (s-matches? (eval `(rx bol (? ">") (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ space) (or "∷" "::")))
                 (buffer-string))
     ;; At an equation?
     (s-matches? (eval `(rx bol (? ">") (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ nonl) "="))
                 (cb-buffers-current-line)))))

(defun cb-haskell-meta-ret--at-record-decl-data-header? ()
  (when (s-matches? (rx bol (? ">") (* space) "data" space) (cb-buffers-current-line))
    (shm/reparse)
    (save-excursion
      (back-to-indentation)
      (shm/goto-parent-end)
      (s-matches? "}" (cb-buffers-current-line)))))

(defun cb-haskell-meta-ret--at-end-of-record-decl? ()
  (save-excursion
    (goto-char (line-beginning-position))
    (and (cb-haskell-meta-ret--in-data-decl?)
         (s-matches? (rx "}" (* space) eol) (cb-buffers-current-line)))))

(defun cb-haskell-meta-ret--insert-record-field ()
  (let ((underscore-prefix-style?
         (s-matches? (rx bol (? ">") (* space) (? (or "{" ",")) (* space) "_") (cb-buffers-current-line)))

        (inserting-first-field? (sp-inside-curly-braces-blank-content?))

        (brace-or-comma-column
         (save-excursion
           (goto-char (line-beginning-position))
           (cond ((and (search-forward-regexp (rx (or "," "{")) nil t)
                       (sp-inside-curly-braces?))
                  (forward-char -1)
                  (current-column))
                 (t 2)))))

    (goto-char (line-end-position))
    (if inserting-first-field?
        (just-one-space)
      (newline)
      (indent-to-column brace-or-comma-column))

    (yas-expand-snippet
     (format "%s%s${1:field} :: ${2:T}"
             (if inserting-first-field? "" ", ")
             (if underscore-prefix-style? "_" "")))))

(defun cb-haskell-meta-ret--insert-function-template (fname)
  (back-to-indentation)
  (when (thing-at-point-looking-at "where")

    (evil-forward-word-begin))
  (let ((col 0))
    (setq col (current-column))
    (shm/reparse)
    (shm/goto-parent-end)

    (goto-char (line-end-position))
    (newline)
    (when (s-ends-with? ".lhs" (buffer-file-name))
      (insert "> "))
    (indent-to col)
    (yas-expand-snippet (format "%s $0" fname) nil nil '((yas/indent-line 'fixed)))))

(defun cb-haskell-meta-ret--insert-deriving-clause ()
  (goto-char (line-end-position))
  (when (s-matches? (rx (not space) (* space) "}" (* space) eol)
                    (cb-buffers-current-line))
    (search-backward "}")
    (newline-and-indent)
    (goto-char (line-end-position)))

  (just-one-space)
  (insert "deriving ()")
  (forward-char -1))

;;;###autoload
(defun cb-haskell-meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Append new record field
   ((and (cb-haskell-meta-ret--at-record-decl-data-header?)
         (or (s-matches? (rx "{" (* space) eol) (cb-buffers-current-line))
             (s-matches? (rx bol (* space) "{") (cb-haskell-meta-ret--line-content-relative +1))))
    (search-forward "{")
    (-let [(&plist :end end) (sp-get-enclosing-sexp)]
      (goto-char (1- end))
      (when (sp-inside-curly-braces? t)
        (newline))
      (forward-line -1)
      (cb-haskell-meta-ret--insert-record-field)
      (message "New field")))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (? ">") (* space) "data" (+ space)) (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| ")
    (goto-char (line-beginning-position))
    (haskell-indentation-indent-line)
    (goto-char (line-end-position))
    (message "New data case"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (? ">") (* space) "|") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "| ")
    (message "New data case"))

   ;; Insert new alternative case
   ((s-matches? (rx bol (? ">") (* space) "<|>") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "<|> ")
    (message "New alternative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<$>") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<*>") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new import
   ((s-matches? (rx bol (? ">") (* space) "import") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "import ")
    (message "New import"))

   ;; Insert new record field
   ((and (cb-haskell-meta-ret--in-data-decl?)
         (or (s-matches? (rx bol (? ">") (* space) (or "{") (* space)) (cb-buffers-current-line))
             (s-matches? (rx "{" (* space) eol) (cb-haskell-meta-ret--line-content-relative -1))))
    (cb-haskell-meta-ret--insert-record-field)
    (message "New field"))

   ;; New function case.
   ((cb-haskell-meta-ret--at-decl-for-function? (cb-haskell-meta-ret--first-ident-on-line))
    (back-to-indentation)
    (let ((fname (cb-haskell-meta-ret--first-ident-on-line)))
      (cb-haskell-meta-ret--insert-function-template fname)
      (message "New binding case")))

   ;; Insert deriving clause
   ((cb-haskell-meta-ret--at-end-of-record-decl?)
    (cb-haskell-meta-ret--insert-deriving-clause)
    (message "Deriving clause"))

   ;; Insert new record field
   ((and (cb-haskell-meta-ret--in-data-decl?)
         (s-matches? (rx bol (? ">") (* space) (or "{" ",") (* space)) (cb-buffers-current-line)))
    (cb-haskell-meta-ret--insert-record-field)
    (message "New field"))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (? ">") (* space) ",") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (? ">") (* space) "->") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert "-> ")
    (message "New arrow"))

   ;; Insert new pattern match case below the current one.
   ((s-matches? (rx bol (? ">") (* space) (+ (not (any "="))) "->") (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (yas-expand-snippet "${1:pat} -> $0")
    (message "New pattern match case"))
   ((s-matches? (rx bol (? ">") (* space) "case" (+ space)) (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-expand-snippet "${1:pat} -> $0")
    (message "New pattern match case"))

   ;; Insert new line starting with a comma for the current braced expr
   ((s-matches? (rx bol (? ">") (* space) (or "[" "{")) (cb-buffers-current-line))
    (cb-haskell-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line with a do-binding.
   ((s-matches? (rx bol (? ">") (* space) (+ nonl) "<-") (cb-buffers-current-line))
    (back-to-indentation)
    (let ((col (current-column)))
      (search-forward-regexp (rx  "<-"))
      (shm/forward-node)
      (newline)
      (indent-to col))
    (yas-expand-snippet "${1:name} <- $0")
    (message "New do-binding"))

   (t
    (goto-char (line-end-position))
    (haskell-indentation-newline-and-indent)
    (message "New line")))

  (evil-insert-state))

(provide 'cb-haskell-meta-ret)

;;; cb-haskell-meta-ret.el ends here
