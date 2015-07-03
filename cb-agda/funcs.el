(require 'dash)
(require 's)

(defun agda/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "{-#") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun agda/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" "-}" "#-}"))
              (buffer-substring (point) (line-end-position))))

(defun agda/smart-space ()
  "Perform extra padding inside lists."
  (interactive)
  (cond
   ((and (agda/after-subexpr-opening?) (agda/before-subexp-closing?))
    (just-one-space)
    (save-excursion
      (insert " ")))
   (t
    (insert " "))))

(defun agda/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((and (agda/after-subexpr-opening?)
          (agda/before-subexp-closing?)
          (thing-at-point-looking-at (rx (+ space))))
     (delete-horizontal-space))

    ((and (s-matches? (rx (or "{-#" "{-") eol)
                      (buffer-substring (line-beginning-position) (point)))
          (s-matches? (rx bol (or "#-}" "-}"))
                      (buffer-substring (point) (line-end-position))))
     (atomic-change-group
       (delete-char 1)
       (delete-char -1)))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))


;; Smart M-RET

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

   ((s-matches? (rx bol (* space) "..." (+ space) "|") (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "... | "))

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


;; Unicode support

(defun agda/rewrite-symbols-in-buffer ()
  (--each '(("->" "→")
            ("\\<Nat\\>" "ℕ")
            ("\\<forall\\>" "∀"))
    (apply 'agda/rewrite-symbol it)))

(defun agda/rewrite-symbol (sym repl)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp sym nil t)
        (replace-match repl)))))
