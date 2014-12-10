;;; Smart ops

(defun idris/smart-colon ()
  (interactive)
  (cond
   ((and (char-before) (equal (char-to-string (char-before)) " "))
    (super-smart-ops-insert ":"))
   ((and (char-after) (equal (char-to-string (char-after)) ")"))
    (insert ":"))
   (t
    (super-smart-ops-insert ":"))))

(defun idris/smart-comma ()
  (interactive)
  (cond
   ((s-matches? (rx bol (* space) eol)
                (buffer-substring (line-beginning-position) (point)))
    (insert ", ")
    (idris-indentation-indent-line))
   (t
    (core/comma-then-space))))

(defun idris/smart-pipe ()
  "Insert a pipe operator. Add padding, unless we're inside a list."
  (interactive)
  (let ((in-empty-square-braces?
         (save-excursion
           (-when-let (pair (sp-backward-up-sexp))
             (cl-destructuring-bind (&key op beg end &allow-other-keys) pair
               (and (equal "[" op)
                    (s-blank? (buffer-substring (1+ beg) (1- end)))))))))
    (cond
     (in-empty-square-braces?
      (delete-horizontal-space)
      (insert "| ")
      (save-excursion
        (insert " |"))
      (message "Inserting idiom brackets"))

     (t
      (super-smart-ops-insert "|")))))

(defun idris/smart-dot ()
  "Insert a period with context-sensitive padding."
  (interactive)
  (let ((looking-at-module-or-constructor?
         (-when-let (sym (thing-at-point 'symbol))
           (s-uppercase? (substring sym 0 1)))))
    (cond
     (looking-at-module-or-constructor?
      (insert "."))
     ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
      (insert "."))
     (t
      (super-smart-ops-insert ".")))))

(defun idris/smart-question-mark ()
  "Insert a ? char as an operator, unless point is after an = sign."
  (interactive)
  (cond
   ((s-matches? (rx "=" (* space) eol) (buffer-substring (line-beginning-position) (point)))
    (just-one-space)
    (insert "?"))
   (t
    (super-smart-ops-insert "?"))))

(defun idris/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "[|") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun idris/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" "-}" "|]"))
              (buffer-substring (point) (line-end-position))))


(defun idris/smart-space ()
  "Use shm space, but perform extra padding inside lists."
  (interactive)
  (cond
   ((and (idris/after-subexpr-opening?) (idris/before-subexp-closing?))
    (delete-horizontal-space)
    (insert "  ")
    (forward-char -1)
    )
   (t
    (insert " "))))


;;; Commands

(defun idris/switch-to-src ()
  "Pop to the last idris source buffer."
  (interactive)
  (-if-let (buf (car (--filter-buffers (derived-mode-p 'idris-mode))))
      (pop-to-buffer buf)
    (error "No idris buffers")))
