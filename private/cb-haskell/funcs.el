;; -*- lexical-binding: t; -*-
(require 'dash)
(require 's)
(require 'ert)
(require 'haskell-parser)

(defun haskell/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "{-#" "(#") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun haskell/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" "-}" "#-}" "#)"))
              (buffer-substring (point) (line-end-position))))

(defun haskell/smart-space ()
  "Use shm space, but perform extra padding inside lists."
  (interactive)
  (cond
   ((and (haskell/after-subexpr-opening?) (haskell/before-subexp-closing?))
    (delete-horizontal-space)
    (call-interactively 'shm/space)
    (save-excursion (call-interactively 'shm/space)))
   (t
    (call-interactively 'shm/space))))

(defun haskell/interactive-smart-space ()
  "Use shm space, but perform extra padding inside lists."
  (interactive)
  (if (haskell-interactive-at-compile-message)
      (haskell-interactive-mode-space)
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " "))))


;;; Define smart operators.

(defun haskell/inside-parens? ()
  "Non-nil if point is inside a parenthesised expression."
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (equal (char-after)
             (string-to-char "(")))))

(defun haskell/in-empty-braces? ()
  "Non-nil if point is between empty square or curly braces."
  (and (s-matches? (rx (or "{" "[") (* space) eol)
                   (buffer-substring (line-beginning-position) (point)))
       (s-matches? (rx bol (* space) (or "}" "]"))
                   (buffer-substring (point) (line-end-position)))))

(defun haskell/smart-minus ()
  "Context-sensitive minus character."
  (interactive)
  (cond
   ((and (s-matches? (rx "{" (* space) eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (* space) "}")
                     (buffer-substring (point) (line-end-position))))
    (delete-horizontal-space)
    (insert "- ")
    (save-excursion
      (insert " -")))
   (t
    (super-smart-ops-insert "-"))))

(defun haskell/smart-hash ()
  "Context-sensitive hash character."
  (interactive)
  (cond
   ((and (s-matches? (rx (or "{-" "(") (* space) eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (* space) (or "-}" ")"))
                     (buffer-substring (point) (line-end-position))))
    (delete-horizontal-space)
    (insert "# ")
    (save-excursion
      (insert " #")))
   (t
    (insert "#"))))

(defun haskell/smart-pipe ()
  "Insert a pipe operator. Add padding, unless we're inside a list."
  (interactive)
  (cond
   ((s-matches? (rx "[" (* (any "|" alnum)) eol)
                (buffer-substring (line-beginning-position) (point)))
    (insert "|"))
   ((s-matches? (rx "--" (* space) eol)
                (buffer-substring (line-beginning-position) (point)))
    (just-one-space)
    (insert "|"))
   (t
    (super-smart-ops-insert "|"))))

(defun haskell/looking-at-module-or-constructor? ()
  (-when-let (sym (thing-at-point 'symbol))
    (s-uppercase? (substring sym 0 1))))

(defun haskell/smart-dot ()
  "Insert a period. Add padding, unless this line is an import statement."
  (interactive)
  (cond
   ((thing-at-point-looking-at (rx digit (* space) "."))
    (save-excursion
      (search-backward ".")
      (just-one-space))
    (insert ". "))

   ((thing-at-point-looking-at (rx digit))
    (insert "."))

   ((haskell/looking-at-module-or-constructor?)
    (insert "."))

   ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
    (insert "."))

   ((equal (char-after) (string-to-char "}"))
    (insert "."))

   ((thing-at-point-looking-at (rx "^"))
    (insert "."))

   (t
    (super-smart-ops-insert "."))))

(defun haskell/smart-colon ()
  "Insert a colon, with context-sensitive formatting."
  (interactive)
  (cond
   ((and (haskell/inside-parens?)
         (s-matches? (rx ":" (* space) eol)
                     (buffer-substring (line-beginning-position) (point))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (point))
      (save-excursion
        (search-backward-regexp (rx (* space) ":" (* space)))
        (delete-region (point) (point-max)))
      (insert " :: ")))

   ((haskell/inside-parens?)
    (insert ":"))

   (t
    (super-smart-ops-insert ":"))))

(defun haskell/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((and (haskell/after-subexpr-opening?)
          (haskell/before-subexp-closing?)
          (thing-at-point-looking-at (rx (+ space))))
     (delete-horizontal-space))

    ((and (s-matches? (rx (or "{-#" "{-" "(#") eol)
                      (buffer-substring (line-beginning-position) (point)))
          (s-matches? (rx bol (or "#-}" "-}" "#)"))
                      (buffer-substring (point) (line-end-position))))
     (atomic-change-group
       (delete-char 1)
       (delete-char -1)))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))

(defun haskell/smart-comma ()
  "Insert a comma, with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
     (delete-horizontal-space)
     (insert ",")
     (hi2-indent-line)
     (just-one-space))

    (t
     (core/comma-then-space)))))

(defun haskell/ghci-line-beginning-position ()
  "Narrow to the current line, excluding the ghci prompt."
  (save-excursion
    (cond ((haskell-interactive-at-prompt)
           (goto-char (line-beginning-position))
           (or (search-forward-regexp (s-trim-left haskell-interactive-prompt)
                                      (line-end-position)
                                      t)
               (line-beginning-position)))
          (t
           (line-beginning-position)))))

(defun haskell/ghci-smart-colon ()
  "Insert a smart operator, unless point is immediately after the GHCI prompt."
  (interactive)
  (save-restriction
    (narrow-to-region (haskell/ghci-line-beginning-position)
                      (line-end-position))
    (if (s-blank? (buffer-substring (line-beginning-position) (point)))
        (insert ":")
      (super-smart-ops-insert ":"))))

(defun haskell/ghci-smart-comma ()
  "Insert a comma with padding."
  (interactive)
  (save-restriction
    (narrow-to-region (haskell/ghci-line-beginning-position)
                      (point))
    (unless (s-blank? (current-line))
      (delete-horizontal-space))

    (insert ", ")))


;;; Formatting

(defun haskell/format-dwim ()
  (interactive "*")
  (hindent/reformat-decl)
  (haskell-mode-stylish-buffer))

(defun haskell/ret ()
  "Insert a newline, possibly continuing a comment."
  (interactive "*")
  (if (s-matches? (rx bol (* space) "--") (current-line))
      (insert "\n-- ")
    (shm/simple-indent-newline-same-col)))

(defun haskell/use-unicode-symbols? ()
  (-contains? (haskell/language-pragmas-in-file) "UnicodeSyntax"))

(defun haskell/unicode-before-save ()
  (when (haskell/use-unicode-symbols?)
    (haskell/rewrite-symbols-in-buffer)))

(defun haskell/rewrite-symbols-in-buffer ()
  (--each '(("->" "→")
            ("=>" "⇒")
            ("<-" "←")
            ("::" "∷")
            ("forall" "∀"))
    (apply 'haskell/rewrite-symbol it)))

(defun haskell/rewrite-symbol (sym repl)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp (rx-to-string `(and (or "(" space)
                                                        (group ,sym)
                                                        (or space ")" eol))
                                                  t)
                                    nil t)

        (unless (core/in-string-or-comment?)
          (replace-match repl t t nil 1))))))


;;; Define smart M-RET command

(defun haskell/meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Append new record field
   ((haskell/at-record-decl-data-header?)
    (back-to-indentation)
    (shm/reparse)
    (shm/goto-parent-end)
    (search-backward-regexp (rx (or "," "{")))
    (haskell/insert-record-field)
    (message "New field"))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) "data" (+ space)) (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| ")
    (goto-char (line-beginning-position))
    (hi2-indent-line)
    (goto-char (line-end-position))
    (message "New data case"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "| ")
    (message "New data case"))

   ;; Insert new alternative case
   ((s-matches? (rx bol (* space) "<|>") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "<|> ")
    (message "New alternative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (* space) "<*>") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new import
   ((s-matches? (rx bol (* space) "import") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "import ")
    (message "New import"))

   ;; New function case.
   ((haskell/at-decl-for-function? (haskell/first-ident-on-line))
    (let* ((fname (haskell/first-ident-on-line))
           (parsed (haskell/parse-function-decl fname)))
      (haskell/insert-function-template fname parsed)
      (message "New binding case")))

   ;; Insert deriving clause
   ((haskell/at-end-of-record-decl?)
    (haskell/insert-deriving-clause)
    (message "Deriving clause"))

   ;; Insert new record field
   ((and (haskell/in-data-decl?)
         (s-matches? (rx bol (* space) (or "{" ",") (* space)) (current-line)))
    (haskell/insert-record-field)
    (message "New field"))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (* space) ",") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (* space) (or "→" "->")) (current-line))
    (haskell/newline-indent-to-same-col)
    (insert (format "%s " (haskell/fmt-rarrow)))
    (message "New arrow"))

   ;; Insert new pattern match case below the current one.
   ((s-matches? (rx bol (* space) (+ (not (any "="))) (or "->" "→")) (current-line))
    (haskell/newline-indent-to-same-col)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell/fmt-rarrow)))
    (message "New pattern match case"))
   ((s-matches? (rx bol (* space) "case" (+ space)) (current-line))
    (newline-and-indent)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell/fmt-rarrow)))
    (message "New pattern match case"))

   ;; Insert new line starting with a comma for the current braced expr
   ((s-matches? (rx bol (* space) (or "[" "{")) (current-line))
    (haskell/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line with a do-binding.
   ((s-matches? (rx bol (* space) (+ nonl) (or "<-" "←")) (current-line))
    (back-to-indentation)
    (let ((col (current-column)))
      (search-forward-regexp (rx  (or "<-" "←")))
      (shm/forward-node)
      (newline)
      (indent-to col))
    (yas-expand-snippet (format "${1:name} %s $0" (haskell/fmt-larrow)))
    (message "New do-binding"))

   (t
    (goto-char (line-end-position))
    (hi2-newline-and-indent)
    (message "New line")))

  (evil-insert-state))

(defun haskell/at-record-decl-data-header? ()
  (when (s-matches? (rx bol (* space) "data" space) (current-line))
    (shm/reparse)
    (save-excursion
      (back-to-indentation)
      (shm/goto-parent-end)
      (s-matches? "}" (current-line)))))

(defun haskell/newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (insert str))

(defun haskell/newline-indent-to-same-col ()
  "Make a new line below the current one and indent to the same column."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

(defvar haskell/haskell-keywords
  '("let" "where" "module" "case" "class" "data" "deriving" "default"
    "import" "infixl" "infixr" "newtype" "data" "type" "if" "then" "else"))

(defun haskell/first-ident-on-line ()
  (car (-difference (s-split (rx space) (current-line) t)
                    haskell/haskell-keywords)))

(defun haskell/insert-function-template (fname parsed-typesig)
  (back-to-indentation)
  (when (thing-at-point-looking-at "where")
    (evil-forward-word-begin))
  (let ((col 0))
    (setq col (current-column))
    (shm/reparse)
    (shm/goto-parent-end)

    (goto-char (line-end-position))
    (newline)
    (indent-to col)
    (-let [(&plist :args args) parsed-typesig]
      (cond
       (args
        (let ((args-fmt
               (->> args
                    (--map-indexed (format "${%s:{-%s-}}" (1+ it-index) it))
                    (s-join " "))))
          (yas-expand-snippet (format "%s %s = ${%s:_}"
                                      fname
                                      args-fmt
                                      (1+ (length args))))))
       (t
        (yas-expand-snippet (format "%s = ${1:_}" fname)))))))

(defun haskell/at-decl-for-function? (fname)
  (when fname
    (or
     ;; A type decl exists in this buffer?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ space) (or "∷" "::")))
                 (buffer-string))
     ;; At an equation?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ nonl) "="))
                 (current-line)))))

(defun haskell/back-to-function-typesig (fname)
  (let ((function-decl-rx
         (rx-to-string `(and bol (* space) (? (or "let" "where") (+ space))
                             ,fname (+ space) (or "∷" "::")))))

    (if (s-matches? function-decl-rx (current-line))
        t
      (search-backward-regexp function-decl-rx nil t))))

(defun haskell/in-data-decl? ()
  (cond
   ((core/in-string-or-comment?) nil)
   ((s-matches? "}" (buffer-substring (line-beginning-position) (point))) nil)
   ((thing-at-point-looking-at (rx bol (* space) "data ")) t)
   (t
    (save-excursion
      (when (search-backward-regexp (rx bol (not space)) nil t)
        (thing-at-point-looking-at "data "))))))

(defun haskell/at-end-of-record-decl? ()
  (save-excursion
    (goto-char (line-beginning-position))
    (and (haskell/in-data-decl?)
         (s-matches? (rx "}" (* space) eol) (current-line)))))

(defun haskell/insert-deriving-clause ()
  (goto-char (line-end-position))
  (when (s-matches? (rx (not space) (* space) "}" (* space) eol)
                    (current-line))
    (search-backward "}")
    (newline-and-indent)
    (goto-char (line-end-position)))

  (just-one-space)
  (insert "deriving ()")
  (forward-char -1))

(defun haskell/insert-record-field ()
  (let ((underscore-prefix-style?
         (s-matches? (rx bol (* space) (or "{" ",") (* space) "_") (current-line)))

        (inserting-first-field? (haskell/at-record-with-no-fields?))

        (brace-or-comma-column
         (save-excursion
           (goto-char (line-beginning-position))
           (cond ((search-forward-regexp (rx (or "," "{")) nil t)
                  (forward-char -1)
                  (current-column))
                 (t 2)))))

    (goto-char (line-end-position))
    (if inserting-first-field?
        (just-one-space)
      (newline)
      (indent-to-column brace-or-comma-column))

    (yas-expand-snippet
     (format "%s%s${1:field} %s ${2:T}"
             (if inserting-first-field? "" ", ")
             (if underscore-prefix-style? "_" "")
             (haskell/fmt-::)))))

(defun haskell/at-record-with-no-fields? ()
  (save-excursion
    (search-backward-regexp (rx bol (* space) "data"))
    (let ((limit (save-excursion (forward-line) (line-end-position))))
      (when (search-forward-regexp (rx "{" (not (any "-"))) limit t)
        (-if-let* (((&plist :op op :beg beg :end end) (sp-get-enclosing-sexp))
                   (region (ignore-errors (buffer-substring (1+ beg) (1- end)))))
            (not (s-matches? (rx graphic) region))
          t)))))

(defun haskell/fmt-::     () (if (haskell/use-unicode-symbols?) "∷" "::"))
(defun haskell/fmt-rarrow () (if (haskell/use-unicode-symbols?) "→" "->"))
(defun haskell/fmt-larrow () (if (haskell/use-unicode-symbols?) "←" "<-"))

(defun haskell/parse-function-decl (fname)
  (save-excursion
    (when (haskell/back-to-function-typesig fname)
      (let* ((start (point))
             (end (progn (shm/goto-parent-end) (point)))
             (typesig (buffer-substring-no-properties start end)))

        (haskell-parser-parse-typesig typesig)))))


;;; SHM smart op integration

(defun haskell/shm-handle-deletions (n)
  (when (true? structured-haskell-mode)
    (save-excursion
      (shm-appropriate-adjustment-point 'backward)
      (shm-adjust-dependents (point) (- n)))
    (shm/init t)))

(defun haskell/shm-handle-insertions (n)
  (when (true? structured-haskell-mode)
    (save-excursion
      (shm-appropriate-adjustment-point 'forward)
      (shm-adjust-dependents (point) n))
    (shm/init t)))

(defun haskell/init-shm-smart-ops-compat ()
  "Bind shm commands to smart operator insertions and deletions."
  (add-hook 'super-smart-ops-text-inserted-functions
            'haskell/shm-handle-insertions nil t)
  (add-hook 'super-smart-ops-text-removed-functions
            'haskell/shm-handle-deletions nil t))


;;; Options insertion

(defvar haskell/ghc-options
  '("-fcase-merge"
    "-fcse"
    "-fdefer-type-errors"
    "-fglasgow-exts"
    "-fhelpful-errors"
    "-firrefutable-tuples"
    "-fno-defer-type-errors"
    "-fno-glasgow-exts"
    "-fno-helpful-errors"
    "-fno-implicit-import-qualified"
    "-fno-irrefutable-tuples"
    "-fno-print-bind-contents"
    "-fno-warn-auto-orphans"
    "-fno-warn-deprecated-flags"
    "-fno-warn-duplicate-exports"
    "-fno-warn-hi-shadowing"
    "-fno-warn-identities"
    "-fno-warn-implicit-prelude"
    "-fno-warn-incomplete-patterns"
    "-fno-warn-incomplete-record-updates"
    "-fno-warn-incomplete-uni-patterns"
    "-fno-warn-lazy-unlifted-bindings"
    "-fno-warn-missing-fields"
    "-fno-warn-missing-local-sigs"
    "-fno-warn-missing-methods"
    "-fno-warn-missing-signatures"
    "-fno-warn-monomorphism-restriction"
    "-fno-warn-name-shadowing"
    "-fno-warn-orphans"
    "-fno-warn-overlapping-patterns"
    "-fno-warn-safe"
    "-fno-warn-tabs"
    "-fno-warn-type-defaults"
    "-fno-warn-unrecognised-pragmas"
    "-fno-warn-unsafe"
    "-fno-warn-unused-binds"
    "-fno-warn-unused-do-bind"
    "-fno-warn-unused-imports"
    "-fno-warn-unused-matches"
    "-fno-warn-wrong-do-bind"
    "-fnowarn-missing-import-lists"
    "-fwarn-amp"
    "-fwarn-deprecated-flags"
    "-fwarn-duplicate-constraints"
    "-fwarn-duplicate-exports"
    "-fwarn-hi-shadowing"
    "-fwarn-identities"
    "-fwarn-implicit-prelude"
    "-fwarn-incomplete-patterns"
    "-fwarn-incomplete-record-updates"
    "-fwarn-incomplete-uni-patterns"
    "-fwarn-lazy-unlifted-bindings"
    "-fwarn-missing-fields"
    "-fwarn-missing-import-lists"
    "-fwarn-missing-local-sigs"
    "-fwarn-missing-methods"
    "-fwarn-missing-signatures"
    "-fwarn-monomorphism-restriction"
    "-fwarn-name-shadowing"
    "-fwarn-orphans"
    "-fwarn-overlapping-patterns"
    "-fwarn-safe"
    "-fwarn-tabs"
    "-fwarn-type-defaults"
    "-fwarn-typed-holes"
    "-fwarn-unrecognised-pragmas"
    "-fwarn-unsafe"
    "-fwarn-unused-binds"
    "-fwarn-unused-do-bind"
    "-fwarn-unused-imports"
    "-fwarn-unused-matches"
    "-fwarn-warnings-deprecations"
    "-fwarn-wrong-do-bind"))

(defconst haskell/ghc-opts-regex (rx "{-#" (+ space) "OPTIONS_GHC" (+ space)
                                     (group (*? nonl))
                                     (+ space) "#-}"))

(defun haskell/ghc-options-in-file ()
  (->> (buffer-string)
       substring-no-properties
       (s-match-strings-all haskell/ghc-opts-regex)
       (-map 'cdr)
       (-flatten)
       (--mapcat (s-split (rx (* space) "," (* space)) it))))

(defun haskell/insert-ghc-option (opt)
  "Insert OPT into the GHC options list for the current file."
  (interactive (list (completing-read "GHC Option: " haskell/ghc-options nil t)))
  (let ((cur-opts (haskell/ghc-options-in-file)))
    (if (--any? (s-matches? opt it) cur-opts)
        (user-error "Option %s already set" opt)
      (haskell/set-buffer-ghc-opts (cons opt cur-opts)))))

(defun haskell/set-buffer-ghc-opts (opts)
  (let ((opts (s-join " " (-sort 'string-lessp (-uniq opts)))))
    (save-excursion
      (haskell/delete-ghc-opts)
      (haskell/goto-buffer-start)
      (insert (format "{-# OPTIONS_GHC %s #-}\n" opts)))))

(defun haskell/delete-ghc-opts ()
  (save-excursion
    (haskell/goto-buffer-start)
    (while (search-forward-regexp haskell/ghc-opts-regex nil t)
      (replace-match "")
      (when (s-blank? (current-line))
        (join-line)))))

(defvar haskell//language-pragmas nil)

(defun haskell/language-pragmas ()
  "List the language pragmas available in GHC."
  (unless haskell//language-pragmas
    ;; Retrive list of language pragmas from GHC.
    (let ((str (s-split "\n" (s-trim (shell-command-to-string "ghc --supported-languages")))))
      (setq haskell//language-pragmas str)))

  haskell//language-pragmas)



(defun haskell/language-pragmas-in-file ()
  "List the language pragmas set in the current file."
  (--filter (s-matches? it (buffer-string))
            (haskell/language-pragmas)))

(defun haskell/available-language-pragmas ()
  "List the language pragmas that have not been set in the current file."
  (-difference (haskell/language-pragmas) (haskell/language-pragmas-in-file)))

(defun haskell/insert-language-pragma (pragma)
  "Read a language pragma to be inserted at the start of this file."
  (interactive (list (completing-read "Pragma: "
                                      (haskell/available-language-pragmas)
                                      nil t)))
  (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
    (save-excursion
      (haskell/goto-buffer-start)

      (insert s))))

(defun haskell/goto-buffer-start ()
  (goto-char (point-min))

  ;; Skip #! line
  (when (and (s-matches? (rx bol "#!") (current-line))
             (search-forward "#!" nil t))
    (goto-char (line-end-position))
    (forward-char 1))

  (while (and (not (eobp))
              (s-blank? (current-line)))
    (forward-line 1)))

(defun haskell/parse-module (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (search-forward-regexp (rx bol "exposed-modules: ") nil t)
      (let (start end)
        (setq start (point))
        (setq end (if (search-forward ":" nil t)
                      (progn (beginning-of-line) (point))
                    (point-max)))
        (s-split " " (buffer-substring-no-properties start end) t)))))

(defun haskell/haskell-modules ()
  "Get a list of all Haskell modules known to the current project or GHC."
  (-union '("Control.Applicative")
          (-if-let (session (haskell-session-maybe))
              (haskell-session-all-modules session t)
            (->> (shell-command-to-string "ghc-pkg dump")
                 (s-split "---")
                 (-mapcat 'haskell/parse-module)
                 (-map 's-trim)))))

(defun haskell/do-insert-at-imports (str)
  "Prepend STR to this buffer's list of imported modules."
  (save-excursion
    (haskell/goto-buffer-start)

    (cond
     ;; Move directly to import statements.
     ((search-forward-regexp (rx bol "import") nil t))

     ;; Move past module declaration.
     ((search-forward "module" nil t)
      (search-forward "where")
      (forward-line)
      (beginning-of-line)
      (while (and (s-blank? (current-line))
                  (not (eobp)))
        (forward-line)))

     ;; Otherwise insert on first blank line.
     (t
      (until (or (eobp) (s-blank? (current-line)))
        (forward-line))))

    ;; Insert import statement.
    (beginning-of-line)
    (open-line 1)
    (insert str)))

(defun haskell/module->qualified-name (module)
  "Make a reasonable name for MODULE for use in a qualified import."
  (s-word-initials (-last-item (s-split (rx ".") module))))

(defun haskell/insert-qualified-import (module name)
  "Interactively insert a qualified Haskell import statement for MODULE."
  (interactive
   (let ((m (s-trim (completing-read "Module: " (haskell/haskell-modules)))))
     (list m (s-trim (read-string "As: " (haskell/module->qualified-name m)
                                  t)))))

  (if (s-matches? (rx-to-string `(and "import" (+ space) "qualified" (+ space)
                                      ,module (or space eol)))
                  (buffer-string))
      (when (called-interactively-p nil)
        (message "Module '%s' is already imported" module))

    (haskell/do-insert-at-imports (format "import qualified %s as %s" module name))))

(defun haskell/insert-import (module)
  "Interactively insert a Haskell import statement for MODULE."
  (interactive (list (completing-read "Module: " (haskell/haskell-modules))))

  (if (s-matches? (rx-to-string `(and "import" (+ space) ,module (or space eol)))
                  (buffer-string))
      (when (called-interactively-p nil)
        (message "Module '%s' is already imported" module))

    (haskell/do-insert-at-imports (format "import %s" module))))


;;; Flyspell

(defun haskell/flyspell-verify ()
  "Prevent common flyspell false positives in haskell-mode."
  (and (flyspell-generic-progmode-verify)
       (not (or (s-matches? (rx bol (* space) "{-#") (current-line))
                (s-matches? (rx bol (* space) "foreign import") (current-line))))))

(defun haskell/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'haskell/flyspell-verify))


;;; File template utils

(defun haskell/file-name->module ()
  (-if-let (root (and (buffer-file-name) (projectile-project-p)))

      (->> (f-no-ext (buffer-file-name))
           (s-chop-prefix root)
           f-split
           (-drop 1)
           (--map (let ((x (substring it 0 1))
                        (xs (substring it 1)))
                    (concat (s-upcase x) xs)))
           (s-join "."))

    (s-upper-camel-case (f-filename (f-no-ext (buffer-name))))))

(defun haskell/last-declared-type-name ()
  "Find the last type declared with `data' or `newtype'"
  (save-excursion
    (when (search-backward-regexp (rx bol (* space)
                                      (or "newtype" "data")
                                      (+ space)
                                      (group (+ word)))
                                  nil t))
    (match-string-no-properties 1)))

(defun haskell/last-imported-header ()
  "Find the last header imported by a foreign import decl."
  (save-excursion
    (when (search-backward-regexp (rx bol (* space)
                                      "foreign" (+ space) "import" (+ space)
                                      (* nonl)
                                      "\""
                                      (group (+ graphic)))
                                  nil t)
      (match-string-no-properties 1))))


;;; Commands

(defun haskell/join-line ()
  (interactive)
  (forward-line 1)
  (goto-char (line-beginning-position))
  (call-interactively 'shm/delete-indentation))

(defun cb-haskell/C-c-C-c ()
  (interactive)
  (or (when (fboundp 'ghc-auto) (ghc-auto))
      (haskell-process-cabal-build)))
