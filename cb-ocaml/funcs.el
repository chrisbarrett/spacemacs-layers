;;; funcs.el --- Functions for OCaml layer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'tuareg nil t))

(require 'dash)
(require 'f)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-insert-state "evil-states")
(autoload 'yas-expand-snippet "yasnippet")

(defun cb-ocaml/at-match-header? ()
  (s-matches? (rx "match" (+ nonl) "with" (* space) eol) (cb-buffers-current-line)))

(defun cb-ocaml/at-case? ()
  (s-matches? (rx bol (* space) (or "with" "|") (+ nonl) "->") (cb-buffers-current-line)))

(defun cb-ocaml/at-type-decl? ()
  (or (s-matches? (rx bol (* space) (or "|" "type")) (cb-buffers-current-line))
      (when (s-matches? "=" (cb-buffers-current-line))
        (save-excursion
          (forward-line -1)
          (s-matches? (rx bol (* space) "type") (cb-buffers-current-line))))))

(defun cb-ocaml/at-let-binding? ()
  (s-matches? (rx bol (* space) "let" eow) (cb-buffers-current-line)))

(defun cb-ocaml/at-val-binding? ()
  (s-matches? (rx bol (* space) "val" eow) (cb-buffers-current-line)))

(defun cb-ocaml/at-import? ()
  (s-matches? (rx bol (* space) "open" eow) (cb-buffers-current-line)))

(defun cb-ocaml/at-function-keyword? ()
  (s-matches? (rx bow "function" (* space) eol) (cb-buffers-current-line)))

(defun cb-ocaml/m-ret ()
  (interactive)
  (cond
   ((cb-ocaml/at-import?)
    (goto-char (line-end-position))
    (newline)
    (insert "open "))

   ((cb-ocaml/at-function-keyword?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column (+ col tuareg-function-indent))
      (yas-expand-snippet "| ${1:binding} -> $0")
      (message "Inserted pattern match case.")))

   ((cb-ocaml/at-match-header?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column (+ col tuareg-match-indent))
      (yas-expand-snippet "| ${1:binding} -> $0")
      (message "Inserted pattern match case.")))

   ((cb-ocaml/at-case?)
    (let ((col (if (s-matches? (rx bol (* space) "with" eow) (cb-buffers-current-line))
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
      (indent-to-column (+ col tuareg-type-indent))
      (insert "| ")
      (message "Inserted sum type case.")))

   ((cb-ocaml/at-val-binding?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "val ${1:name} : ${2:type}")
      (message "Inserted val declaration.")))

   ((cb-ocaml/at-let-binding?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "let ${1:name} = $0")
      (message "Inserted let binding.")))

   (t
    (reindent-then-newline-and-indent)))

  (evil-insert-state))

;;; funcs.el ends here
