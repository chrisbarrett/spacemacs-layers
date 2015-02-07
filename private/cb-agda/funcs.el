(require 'dash)
(require 's)

(defvar agda/keywords '("data" "record" "module" "where"))

(defun agda/function-name-at-pt ()
  "Return the name of the function at point."
  (save-excursion
    (search-backward-regexp (rx bol (* space) (group (+ (not (any space ":"))))))
    (let ((s (s-trim (match-string-no-properties 1))))
      (unless (or (-contains? agda/keywords s)
                  (s-blank? s))
        s))))

(defun agda/meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- At functions, create a new case for the function.
- At types, add a 'where' statement if one does not exist.
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond
   ;; Create new function case.
   ((agda/function-name-at-pt)
    (goto-char (line-end-position))
    (let ((fn (agda/function-name-at-pt))
          (col (save-excursion
                 (back-to-indentation)
                 (current-column))))

      (unless (s-matches? (rx bol (* space) eol) (current-line))
        (newline))

      (indent-to-column col)
      (insert fn)
      (just-one-space)))

   ;; Create a new line in a comment.
   ((s-matches? comment-start (current-line))
    (fill-paragraph)
    (comment-indent-new-line)
    (message "New comment line"))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (evil-insert-state))


(defun agda/rewrite-symbols-in-buffer ()
  (--each '(("->" "→")
            ("Nat" "ℕ"))
    (apply 'agda/rewrite-symbol it)))

(defun agda/rewrite-symbol (sym repl)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward sym nil t)
      (replace-match repl))))
