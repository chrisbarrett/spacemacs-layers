(eval-when-compile
  (require 's nil t)
  (require 'dash nil t)
  (require 'smartparens nil t)
  )

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
                      (lambda (&optional arg)
                        (interactive)
                        (with-demoted-errors
                            (sp/insert-or-up ,key arg)))
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
           ((s-matches? (rx (or "(" "[") (* space) eol) bol-to-point)
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

(defun sp/inside-curly-braces? (&optional same-line?)
  (sp/inside-sexp? "{" t))

(defun sp/inside-square-braces? (&optional same-line?)
  (sp/inside-sexp? "[" t))

(defun sp/inside-sexp? (expected-op &optional same-line?)
  (-let [(&plist :beg beg :end end :op actual-op) (sp-get-enclosing-sexp)]
    (when (equal expected-op actual-op)
      (if same-line?
          (= (line-number-at-pos beg) (line-number-at-pos end))
        t))))

(defun sp/beg ()
  (plist-get (sp-get-enclosing-sexp ) :beg))

(defun sp/inside-curly-braces-no-content? (&optional same-line?)
  (-let [(&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)]
    (when (equal op "{")
      (and (s-blank? (buffer-substring (1+ beg) (1- end)))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp/inside-curly-braces-blank-content? (&optional same-line?)
  (-let [(&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)]
    (when (equal op "{")
      (and (s-blank? (s-trim (buffer-substring (1+ beg) (1- end))))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp/inside-curly-braces-with-content? (&optional same-line?)
  (and (sp/inside-curly-braces? same-line?) (not (sp/inside-curly-braces-no-content? same-line?))))

(defun sp/just-after-open-op? (op)
  (s-matches? (rx-to-string `(and ,op (* space) eos)) (buffer-substring (line-beginning-position) (point))))

(defun sp/split-braced-expression-over-new-lines (&optional statement-delimiter-rx)
  "Split the braced expression on the current line over several lines.

Optionally split the internal lines according to the given regexp
STATEMENT-DELIMETER-RX."
  (-let [(&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (newline-and-indent)
      (let ((beg (sp/beg)))
        (while (and (search-forward-regexp statement-delimiter-rx nil t)
                    (<= beg (sp/beg)))
          (when (equal beg (sp/beg))
            (unless (core/in-string-or-comment?)
              (insert "\n")
              (indent-according-to-mode))))))

    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (sp/just-after-open-op? op)
      (forward-line)
      (back-to-indentation))))

(defun sp/generic-prog-backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((sp/inside-curly-braces-no-content?)
     (call-interactively 'sp-backward-delete-char))

    ((sp/inside-curly-braces-blank-content? t)
     (delete-horizontal-space))

    ((sp/inside-curly-braces-blank-content?)
     (just-one-space -1)
     (save-excursion
       (insert " ")))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))

(defun sp/generic-prog-space ()
  "Insert a space, performing extra padding inside braced expressions."
  (interactive)
  (cond
   ((sp/inside-curly-braces-no-content?)
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun sp/generic-prog-ret (&optional arg)
  "Insert a newline with context-sensitive formatting."
  (interactive "P")
  (cond
   ((or arg (core/in-string-or-comment?))
    (comment-indent-new-line)
    (just-one-space))

   ((or (sp/inside-curly-braces? t)
        (sp/inside-square-braces? t))
    (sp/split-braced-expression-over-new-lines (rx (or ";" ","))))

   (t
    (call-interactively 'comment-indent-new-line))))


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

(defun sp/scala-curly-brace-padding (id action context)
  "Insert internal and external padding."
  (if (s-matches? (rx bol (* space) "import" eow) (current-line))
      (sp/internal-padding id action context)
    (and (sp/external-padding id action context)
         (sp/internal-padding id action context))))
