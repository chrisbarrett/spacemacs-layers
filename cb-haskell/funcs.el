;; -*- lexical-binding: t; -*-
(require 'dash)
(require 's)
(require 'ert)
(require 'haskell-parser nil t)

(defun haskell/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "{-#" "(#" "{-@") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun haskell/before-subexp-closing? ()
  (s-matches? (rx bol (? ">") (* space) (or "}" "]" "-}" "#-}" "@-}" "#)"))
              (buffer-substring (point) (line-end-position))))

(defun haskell/smart-space ()
  "Use shm space, but perform extra padding inside lists."
  (interactive)
  (cond
   ((and (haskell/after-subexpr-opening?) (haskell/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (sp/generic-prog-space))))

(defun haskell/interactive-smart-space ()
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (haskell-interactive-mode-space))
   ((and (haskell/after-subexpr-opening?) (haskell/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun haskell/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (cond
   ((and (haskell/after-subexpr-opening?)
         (haskell/before-subexp-closing?)
         (thing-at-point-looking-at (rx (+ space))))
    (delete-horizontal-space))

   ((and (s-matches? (rx (or "{-#" "{-@" "{-" "(#") eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (? ">") (or "@-}" "#-}" "-}" "#)"))
                     (buffer-substring (point) (line-end-position))))
    (delete-char 1)
    (delete-char -1))

   (t
    (sp/generic-prog-backspace))))


;;; Formatting

(defun haskell/format-dwim ()
  (interactive "*")
  (hindent/reformat-decl)
  (haskell-mode-stylish-buffer)
  (haskell/unicode-buffer))

(defun haskell/ret (&optional arg)
  "Insert a newline with context-sensitive formatting.

With prefix arg ARG, just insert a newline and indent."
  (interactive "P")
  (let ((sexp (sp-get-enclosing-sexp)))
    (cond
     (arg
      (newline-and-indent))

     ((s-matches? (rx bol (? ">") (* space) "--") (current-line))
      (insert "\n-- "))

     ((or (sp/inside-curly-braces? t sexp)
          (sp/inside-square-braces? t sexp))
      (haskell/split-braced-expression-over-new-lines sexp))

     ((and (or (sp/inside-curly-braces? nil sexp)
               (sp/inside-square-braces? nil sexp))
           (thing-at-point-looking-at (rx (or "[" "{") (* space))))
      (goto-char (1+ (sp/beg sexp)))
      (newline-and-indent)
      (insert "  "))

     ((or (sp/inside-curly-braces? t sexp)
          (sp/inside-square-braces? t sexp))
      (sp/split-braced-expression-over-new-lines (rx (or ";" ","))))

     (t
      (call-interactively 'haskell-indentation-newline-and-indent)))))

(defun haskell/split-braced-expression-over-new-lines (sexp)
  "Split the braced expression on the current line over several lines."
  (-let [(&plist :beg beg :end end :op op) sexp]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (just-one-space)
      (let ((beg (sp/beg)))
        (while (and (search-forward-regexp (rx ",") nil t)
                    (<= beg (sp/beg)))
          (when (equal beg (sp/beg))
            (unless (core/in-string-or-comment?)
              (forward-char -1)
              (insert "\n")
              (indent-according-to-mode)
              (search-forward ",")
              (just-one-space))))))

    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (sp/just-after-open-op? op)
      (forward-line)
      (back-to-indentation))))

(defun haskell/use-unicode-symbols? ()
  (-contains? (haskell/language-pragmas-in-file) "UnicodeSyntax"))

(defun haskell/unicode-buffer (&optional force)
  (interactive "P")
  (when (or force (haskell/use-unicode-symbols?))
    (--each '(("->" "→")
              ("=>" "⇒")
              ("<-" "←")
              ("::" "∷")
              ("forall" "∀"))
      (apply 'haskell/rewrite-symbol it))))

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
   ((and (haskell/at-record-decl-data-header?)
         (or (s-matches? (rx "{" (* space) eol) (current-line))
             (s-matches? (rx bol (* space) "{") (line-content-relative +1))))
    (search-forward "{")
    (-let [(&plist :end end) (sp-get-enclosing-sexp)]
      (goto-char (1- end))
      (when (sp/inside-curly-braces? t)
        (newline))
      (forward-line -1)
      (haskell/insert-record-field)
      (message "New field")))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (? ">") (* space) "data" (+ space)) (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| ")
    (goto-char (line-beginning-position))
    (haskell-indentation-indent-line)
    (goto-char (line-end-position))
    (message "New data case"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (? ">") (* space) "|") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "| ")
    (message "New data case"))

   ;; Insert new alternative case
   ((s-matches? (rx bol (? ">") (* space) "<|>") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "<|> ")
    (message "New alternative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<$>") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<*>") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new import
   ((s-matches? (rx bol (? ">") (* space) "import") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "import ")
    (message "New import"))

   ;; Insert new record field
   ((and (haskell/in-data-decl?)
         (or (s-matches? (rx bol (? ">") (* space) (or "{") (* space)) (current-line))
             (s-matches? (rx "{" (* space) eol) (line-content-relative -1))))
    (haskell/insert-record-field)
    (message "New field"))

   ;; New function case.
   ((haskell/at-decl-for-function? (haskell/first-ident-on-line))
    (back-to-indentation)
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
         (s-matches? (rx bol (? ">") (* space) (or "{" ",") (* space)) (current-line)))
    (haskell/insert-record-field)
    (message "New field"))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (? ">") (* space) ",") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (? ">") (* space) (or "→" "->")) (current-line))
    (haskell/newline-indent-to-same-col)
    (insert (format "%s " (haskell/fmt-rarrow)))
    (message "New arrow"))

   ;; Insert new pattern match case below the current one.
   ((s-matches? (rx bol (? ">") (* space) (+ (not (any "="))) (or "->" "→")) (current-line))
    (haskell/newline-indent-to-same-col)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell/fmt-rarrow)))
    (message "New pattern match case"))
   ((s-matches? (rx bol (? ">") (* space) "case" (+ space)) (current-line))
    (newline-and-indent)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell/fmt-rarrow)))
    (message "New pattern match case"))

   ;; Insert new line starting with a comma for the current braced expr
   ((s-matches? (rx bol (? ">") (* space) (or "[" "{")) (current-line))
    (haskell/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line with a do-binding.
   ((s-matches? (rx bol (? ">") (* space) (+ nonl) (or "<-" "←")) (current-line))
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
    (haskell-indentation-newline-and-indent)
    (message "New line")))

  (evil-insert-state))

(defun haskell/at-record-decl-data-header? ()
  (when (s-matches? (rx bol (? ">") (* space) "data" space) (current-line))
    (shm/reparse)
    (save-excursion
      (back-to-indentation)
      (shm/goto-parent-end)
      (s-matches? "}" (current-line)))))

(defun haskell/newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (when (s-ends-with? ".lhs" (buffer-name))
    (insert "> "))
  (indent-to col)
  (insert str))

(defun haskell/newline-indent-to-same-col ()
  "Make a new line below the current one and indent to the same column."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (when (s-ends-with? ".lhs" (buffer-name))
      (insert "> "))
    (indent-to col)))

(defvar haskell/haskell-keywords
  '("let" "where" "module" "case" "class" "data" "deriving" "default"
    "import" "infixl" "infixr" "newtype" "data" "type" "if" "then" "else"))

(defun haskell/first-ident-on-line ()
  (car (-difference (s-split (rx (? ">") space) (current-line) t)
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
    (when (s-ends-with? ".lhs" (buffer-file-name))
      (insert "> "))

    (indent-to col)
    (-let [(&plist :args args) parsed-typesig]
      (cond
       (args
        (let ((args-fmt
               (->> args
                    (--map-indexed (format "${%s:x%s}" (1+ it-index) it-index))
                    (s-join " "))))
          (yas-expand-snippet (format "%s %s = ${%s:undefined}"
                                      fname
                                      args-fmt
                                      (1+ (length args))))))
       (t
        (yas-expand-snippet (format "%s = $0" fname)))))))

(defun haskell/at-decl-for-function? (fname)
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
                 (current-line)))))

(defun haskell/back-to-function-typesig (fname)
  (let ((function-decl-rx
         (rx-to-string `(and bol (? ">") (* space) (? (or "let" "where") (+ space))
                             (group-n 1 ,fname) (+ space) (or "∷" "::")))))
    (cond
     ((s-matches? function-decl-rx (current-line))
      (goto-char (line-beginning-position))
      (search-forward-regexp function-decl-rx)
      (goto-char (match-beginning 1)))
     ((search-backward-regexp function-decl-rx nil t)
      (goto-char (match-beginning 1))))))

(defun haskell/in-data-decl? ()
  (cond
   ((core/in-string-or-comment?) nil)
   ((s-matches? "}" (buffer-substring (line-beginning-position) (point))) nil)
   ((thing-at-point-looking-at (rx bol (? ">") (* space) "data ")) t)
   (t
    (save-excursion
      (when (search-backward-regexp (rx bol (? ">") (not space)) nil t)
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
         (s-matches? (rx bol (? ">") (* space) (? (or "{" ",")) (* space) "_") (current-line)))

        (inserting-first-field? (sp/inside-curly-braces-blank-content?))

        (brace-or-comma-column
         (save-excursion
           (goto-char (line-beginning-position))
           (cond ((and (search-forward-regexp (rx (or "," "{")) nil t)
                       (sp/inside-curly-braces?))
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

(defun haskell/fmt-::     () (if (haskell/use-unicode-symbols?) "∷" "::"))
(defun haskell/fmt-rarrow () (if (haskell/use-unicode-symbols?) "→" "->"))
(defun haskell/fmt-larrow () (if (haskell/use-unicode-symbols?) "←" "<-"))

(defun haskell/parse-function-decl (fname)
  (save-excursion
    (when (haskell/back-to-function-typesig fname)
      (shm/reparse)
      (let* ((start (point))
             (end (progn (shm/goto-parent-end) (point)))
             (end (if (= end (point)) (line-end-position) end))
             (typesig (buffer-substring-no-properties start end)))

        (haskell-parser-parse-typesig typesig)))))


;;; Options insertion

(defconst haskell/ghc-options
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
    "-fno-warn-unticked-promoted-constructors"
    "-fno-warn-unused-binds"
    "-fno-warn-unused-do-bind"
    "-fno-warn-unused-imports"
    "-fno-warn-unused-matches"
    "-fno-warn-wrong-do-bind"
    "-fno-warn-missing-import-lists"
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
    "-fwarn-unticked-promoted-constructors"
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
  (--filter (s-matches?
             (rx-to-string `(and "{-# LANGUAGE" (+ space) (* nonl) ,it))
             (buffer-string))
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
     ((search-forward-regexp (rx bol (? ">") "import") nil t))

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
  (-last-item (s-split (rx ".") module)))

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
       (not (or (s-matches? (rx bol (? ">") (* space) "{-#") (current-line))
                (s-matches? (rx bol (? ">") (* space) "foreign import") (current-line))))))

(defun haskell/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'haskell/flyspell-verify))


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
