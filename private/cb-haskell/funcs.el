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
  "Either refill the current comment or string, or prettify the buffer."
  (interactive "*")
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?
           (fill-paragraph)
           (message "Filled paragraph."))
          (t
           (haskell-mode-stylish-buffer)
           (message "Reformatted buffer.")))))


;;; hi2

(defun haskell/show-hi2-guides ()
  (when (true? hi2-mode)
    (hi2-enable-show-indentations)))

(defun haskell/hide-hi2-guides ()
  (when (true? hi2-mode)
    (hi2-disable-show-indentations)))


;;; Define smart M-RET command

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

(defun haskell/insert-function-template (fname)
  (back-to-indentation)

  (when (shm-current-node)
    (shm/goto-parent-end))

  (goto-char (line-end-position))
  (newline)
  (shm-insert-string (concat fname " ")))

(defun haskell/at-decl-for-function? (fname)
  (when fname
    (or
     ;; A type decl exists in this buffer?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ space) "::"))
                 (buffer-string))
     ;; At an equation?
     (s-matches? (eval `(rx bol (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ nonl) "="))
                 (current-line)))))

(defun haskell/start-col-of-string-on-line (str)
  "Return the column where STR starts on this line."
  (when str
    (save-excursion
      (goto-char (line-beginning-position))
      (search-forward str)
      (goto-char (match-beginning 0))
      (current-column))))

(defun haskell/meta-ret (&optional arg)
  "Open a new line in a context-sensitive way.

Arg modifies the thing to be inserted."
  (interactive "P")
  (yas-exit-all-snippets)
  (cond

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

   ;; Insert pattern match at function definition.
   ((s-matches? (rx bol (* space) (+ (not space)) (+ space) "::") (current-line))
    (haskell/insert-function-template (haskell/first-ident-on-line))
    (message "New function case"))

   ;; Insert new pattern match case below the current one.
   ((or (s-matches? (rx bol (* space) (+ (not (any "="))) "->") (current-line))
        (s-matches? (rx bol (* space) "case" (+ space)) (current-line)))
    (haskell/newline-indent-to-same-col)
    (yas-insert-first-snippet (lambda (sn)
                                (equal "match-case" (yas--template-name sn))))
    (message "New pattern match case"))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (* space) ",") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (* space) "->") (current-line))
    (haskell/newline-indent-to-same-col)
    (insert "-> ")
    (message "New arrow"))

   ;; Insert new line with a do-binding or return.
   ((s-matches? (rx bol (* space) (+ nonl) "<-") (current-line))
    (back-to-indentation)
    (let ((col (current-column)))
      (search-forward "<-")
      (shm/forward-node)
      (newline)
      (indent-to col))
    (cond (arg
           (insert "return ")
           (message "Return statement"))
          (t
           (yas-insert-first-snippet (lambda (sn)
                                       (equal "do-binding" (yas--template-name sn))))
           (message "New do-binding"))))

   ;; New function case.
   ((haskell/at-decl-for-function? (haskell/first-ident-on-line))
    (let* ((ident (haskell/first-ident-on-line))
           (col (haskell/start-col-of-string-on-line ident)))
      (haskell/insert-function-template ident)
      (save-excursion
        (back-to-indentation)
        (indent-to col)))
    (message "New binding case"))

   (t
    (goto-char (line-end-position))
    (hi2-newline-and-indent)
    (message "New line")))

  (evil-insert-state))


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

(defun haskell/get-ghc-options-in-file ()
  (->> (buffer-string)
    substring-no-properties
    (s-match-strings-all haskell/ghc-opts-regex)
    (-map 'cdr)
    (-flatten)
    (--mapcat (s-split (rx (* space) "," (* space)) it))))

(defun haskell/insert-ghc-option (opt)
  "Insert OPT into the GHC options list for the current file."
  (interactive (list (ido-completing-read "GHC Option: " haskell/ghc-options nil t)))
  (save-excursion
    (let* ((cur (haskell/get-ghc-options-in-file))
           (opts (s-join " " (-sort 'string-lessp (-union (list opt) cur)))))

      (goto-char (point-min))
      (while (search-forward-regexp haskell/ghc-opts-regex nil t)
        (replace-match ""))

      (goto-char (point-min))
      (insert (format "{-# OPTIONS_GHC %s #-}" opts))
      (when (s-matches? (rx (+ nonl)) (buffer-substring (point) (line-end-position)))
        (newline)))))

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
  (interactive (list (ido-completing-read "Pragma: "
                                          (haskell/available-language-pragmas)
                                          nil t)))
  (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
    (save-excursion
      (goto-char (point-min))
      (insert s))))

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
  (-if-let (session (haskell-session-maybe))
      (haskell-session-all-modules session t)
    (->> (shell-command-to-string "ghc-pkg dump")
      (s-split "---")
      (-mapcat 'haskell/parse-module)
      (-map 's-trim))))

(defun haskell/do-insert-at-imports (str)
  "Prepend STR to this buffer's list of imported modules."
  (save-excursion
    (goto-char (point-min))

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
   (let ((m (s-trim (ido-completing-read "Module: " (haskell/haskell-modules)))))
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
  (interactive (list (ido-completing-read "Module: " (haskell/haskell-modules))))

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

    (s-upper-camel-case (f-no-ext (buffer-name)))))

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
