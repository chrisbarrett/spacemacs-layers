;;; Move up and reformat parens when closing.

(defun sp/insert-or-up (delim &optional arg)
  "Insert a delimiter DELIM if inside a string, else move up.
Prefix ARG is passed to `sp-up-sexp'."
  (interactive "sDelimiter:\nP")
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?
           (insert delim))
          (smartparens-mode
           (sp-up-sexp arg 'interactive))
          (t
           (insert delim)))))

(defun sp/hacky-set-sp-bindings ()
  (cl-loop for key in '(")" "]" "}")
           for map in '(smartparens-mode-map smartparens-strict-mode-map)
           do (eval `(bind-key
                      (kbd key)
                      (command (with-demoted-errors
                                   (sp/insert-or-up ,key _arg)))
                      ,map))))


;;; Use smartparens in certain minibuffer contexts.

(defvar sp/minibuffer-enabled-commands
  '(eval-expression calc-algebraic-entry quick-calc)
  "Commands that take input in the minibuffer for which smartparens should be used.")

(defun sp/maybe-enable-smartparens ()
  (smartparens-mode
   (if (-contains? sp/minibuffer-enabled-commands this-command) +1 -1)))


;;; Utility commands

(defun sp/kill-blank-lines ()
  (interactive)
  (cond
   ((s-blank? (s-trim (current-line)))
    (kill-whole-line))
   (t
    (call-interactively 'sp-kill-sexp)

    ;; Delete extra spaces backwards.
    (when (s-matches? (rx (not space))
                      (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space t))

    ;; Join lines cleanly.
    (when (s-blank? (s-trim (current-line)))
      (let ((pt (point)))
        (join-line)
        (goto-char (1+ pt)))))))

(defun sp/internal-and-external-padding (id action context)
  "Insert internal and external padding."
  (and (sp/external-padding id action context)
       (sp/internal-padding id action context)))

(defun sp/external-padding (id action ctx)
  "Add external padding around ID.
Insert leading padding unless at start of line or after an open round paren."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (save-excursion
      (when (search-backward (sp-get-pair id :open)
                             (line-beginning-position) t)
        (let ((bol-to-point (buffer-substring (line-beginning-position) (point))))
          (cond
           ((s-matches? (rx bol (* space) eol) bol-to-point))
           ((s-matches? (rx "(" (* space) eol) bol-to-point)
            (delete-horizontal-space))
           (t
            (just-one-space))))
        t))))

(defun sp/internal-padding (id action ctx)
  "Add internal padding around ID."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (when (search-backward (sp-get-pair id :open)
                           (line-beginning-position) t)

      (goto-char (match-end 0))
      (insert "  ")
      (forward-char -1))))

(defun sp/just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))


;;; OCaml utils

(defun sp/ml-just-one-space (id action ctx)
  "Pad delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (sp/ml-just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is the first position of another list
    ;; 2. this form begins a new line.
    ;; 3. this form is preceded by a `?`, as in a let binding.
    ;; 4. this form is preceded by a `:`, as in a keyword argument
    ;; 4. this form is preceded by a `.`, as in an array index expression
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "." "," ":" "(" "[" "[|" "{" "?")
                       ;; HACK: utop prompt
                       (and "utop[" (+ digit) "]" ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (s-matches? (rx (or (any ")" "]" "|]" "}") eol))
                          (char-to-string (char-after)))
        (just-one-space)))))


;;; Lisp utils

(defun sp/lisp-just-one-space (id action ctx)
  "Pad Lisp delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (sp/just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is a quoted form
    ;; 2. this is the first position of another list
    ;; 3. this form begins a new line.
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "," "`" "'" "@" "#" "~" "(" "[" "{")
                       ;; HACK: nREPL prompt
                       (and (any alnum "." "/" "-") ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (-contains? '(")" "]" "}") (char-to-string (char-after)))
        (just-one-space)))))


;;; C utils

(defun sp/c-format-after-brace (_id action context)
  "Apply formatting after a brace insertion."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               ;; Search backward for flow control keywords.
               (search-backward "{")
               (or (thing-at-point-looking-at
                    (rx symbol-start (or "else" "do")))
                   (progn
                     (sp-previous-sexp)
                     (thing-at-point-looking-at
                      (rx symbol-start (or "if" "for" "while")))))))
    ;; Insert a space for padding.
    (save-excursion
      (search-backward "{")
      (just-one-space))
    ;; Put braces on new line.
    (newline)
    (save-excursion (newline-and-indent))
    (c-indent-line)))

(defun sp/c-format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               (search-backward "(")
               (thing-at-point-looking-at
                (rx symbol-start (or "=" "return" "if" "while" "for")
                    (* space)))))
    (save-excursion
      (search-backward "(")
      (just-one-space))))


;;; Ruby utils

(defun sp/ruby-should-insert-pipe-close (_id _action _ctx)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair."
  (thing-at-point-looking-at
   (rx-to-string `(and (or "do" "{") (* space) "|"))))

(defun sp/ruby-sp-hook-space-before (_id action _ctx)
  "Move to point before ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-backward "|")
      (just-one-space))))

(defun sp/ruby-sp-hook-space-after (_id action _ctx)
  "Move to point after ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-forward "|")
      (just-one-space))))


;;; Scala utils

(defun sp/scala-format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               (search-backward "(")
               (thing-at-point-looking-at
                (rx symbol-start (or "=" "return" "if" "while" "for" "case")
                    (* space)))))
    (save-excursion
      (search-backward "(")
      (just-one-space))))
