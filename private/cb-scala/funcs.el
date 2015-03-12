;;; Smart ops  -*- lexical-binding: t; -*-

(require 's)
(require 'dash)
(require 'noflet)

(defun scala/equals ()
  (interactive)
  (super-smart-ops-insert "="))

(defun scala/colon ()
  (interactive)
  (core/insert-smart-op-no-leading-space ":"))

(defun scala/repl-colon ()
  (interactive)
  (if (s-matches? (rx bol (* space) "scala>" (* space))
                  (buffer-substring (line-beginning-position) (point)))
      (insert ":")
    (core/insert-smart-op-no-leading-space ":")))

(defun scala/insert-variance-op (op)
  "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
  (cond
   ;; No padding at the start of type parameter.
   ((thing-at-point-looking-at (rx "[" (* space)))
    (delete-horizontal-space)
    (insert op))
   ;; Leading padding after a comma, e.g. for a type parameter or function call.
   ((thing-at-point-looking-at (rx "," (* space)))
    (just-one-space)
    (insert op))
   ;; Otherwise leading and trailing padding.
   (t
    (super-smart-ops-insert op))))

(defun scala/minus ()
  (interactive "*")
  (scala/insert-variance-op "-"))

(defun scala/plus ()
  (interactive "*")
  (scala/insert-variance-op "+"))

(defun scala/slash ()
  "Insert a slash as a smart op.
Typing three in a row will insert a ScalaDoc."
  (interactive "*")
  (if (s-matches? (rx bol (* space) "//" (* space) eol) (current-line))
      (atomic-change-group
        (delete-region (line-beginning-position) (line-end-position))
        (scala/insert-scaladoc))
    (super-smart-ops-insert "/")))

(defun scala/insert-scaladoc ()
  "Insert the skeleton of a ScalaDoc at point."
  (interactive "*")
  (indent-for-tab-command) (insert "/**")
  (core/open-line-below-current-indentation) (insert " * ")
  (save-excursion
    (core/open-line-below-current-indentation) (insert "*/")))

(defun scala/insert-space-before-colon ()
  (save-excursion
    (when (search-backward-regexp (rx (not word) (group (+ ":")) (* space))
                                  (line-beginning-position) t)
      (goto-char (match-beginning 1))
      (just-one-space))))

(defadvice super-smart-ops-insert (before padding-for-colon-op activate)
  "Insert padding before colon if inserting another operator after."
  (when (derived-mode-p 'scala-mode)
    (scala/insert-space-before-colon)))


;;; Interactive

(defun scala/switch-to-src ()
  "Switch back to the last scala source file."
  (interactive)
  (-when-let (buf (car (--filter-buffers (derived-mode-p 'scala-mode))))
    (pop-to-buffer buf)))

(defun scala/load-buffer (file)
  "Load FILE, starting an inferior scala if needed."
  (interactive (list (buffer-file-name)))
  (unless (get-buffer ensime-inf-buffer-name)
    (ensime-inf-run-scala))
  (ensime-inf-load-file file))


;;; Smart editing commands

(defun scala/after-open-curly? ()
  (s-matches? (rx "{" (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/before-close-curly? ()
  (s-matches? (rx bos (* space) "}")
              (buffer-substring (point) (line-end-position))))

(defun scala/between-empty-curly-braces? ()
  (and (scala/after-open-curly?) (scala/before-close-curly?)))

(defun scala/between-curly-braces-with-content? ()
  (or (and (scala/after-open-curly?)
           (s-matches? (rx (+ (not space)) (* space) "}")
                       (buffer-substring (point) (line-end-position))))
      (and (scala/before-close-curly?)
           (s-matches? (rx "{" (* space) (+ (not space)))
                       (buffer-substring (line-beginning-position) (point))))))

(defun scala/split-braced-expression-over-new-lines ()
  "Split the braced expression on the current line over several lines."
  (-let [(&plist :beg beg :end end) (sp-get-enclosing-sexp)]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (newline-and-indent)
      (while (search-forward ";" (line-end-position) t)
        (replace-match "\n")))))

(defun scala/after-lambda-arrow? ()
  (s-matches? (rx (* space) "=>" (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/expand-brace-group-for-hanging-lambda ()
  (scala/split-braced-expression-over-new-lines)
  (goto-char (plist-get (sp-get-enclosing-sexp) :beg))
  (scala/join-line)
  (goto-char (line-end-position))
  (newline-and-indent))

(defun scala/ret (&optional arg)
  "Insert a newline with context-sensitive formatting."
  (interactive "P")
  (cond
   ((scala/at-scaladoc?)
    (goto-char (line-end-position))
    (core/open-line-below-current-indentation)
    (insert "* "))

   ((or arg (core/in-string-or-comment?))
    (comment-indent-new-line)
    (just-one-space))

   ((scala/between-empty-curly-braces?)
    (scala/split-braced-expression-over-new-lines)
    (forward-line)
    (indent-for-tab-command))

   ((and (scala/after-lambda-arrow?) (scala/between-curly-braces-with-content?))
    (scala/expand-brace-group-for-hanging-lambda))

   ((scala/between-curly-braces-with-content?)
    (delete-horizontal-space)
    (scala/split-braced-expression-over-new-lines)
    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (scala/after-open-curly?)
      (forward-line)
      (back-to-indentation)))
   (t
    (call-interactively 'comment-indent-new-line))))

(defun scala/at-scaladoc? ()
  (s-matches? (rx bol (* space) (? "/") (+ "*")) (current-line)))

(defun scala/at-case-class? ()
  (s-matches? (rx bol (* space) "case" (+ space) "class" eow) (current-line)))

(defun scala/at-abstract-sealed-class? ()
  (s-matches? (rx bol (* space) "abstract" (+ space) "sealed" (+ space) "class" eow) (current-line)))

(defun scala/meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- In match statements
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "var" eow) (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "var" (yas--template-name sn))))
    (message "New var binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "val" eow) (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "val" (yas--template-name sn))))
    (message "New val binding"))

   ;; Insert new case class.
   ((or (scala/at-case-class?) (scala/at-abstract-sealed-class?))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case class" (yas--template-name sn))))
    (message "New case class"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "case") (current-line))
    (core/open-line-below-current-indentation)
    (yas-insert-first-snippet (lambda (sn) (equal "case" (yas--template-name sn))))
    (message "New data case"))

   ;; Insert new import statement
   ((s-matches? (rx bol (* space) "import" eow) (current-line))
    (core/open-line-below-current-indentation)
    (insert "import ")
    (message "New import statement"))

   (t
    (goto-char (line-end-position))
    (scala/ret)))

  (evil-insert-state))

(defun scala/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "(") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" ")"))
              (buffer-substring (point) (line-end-position))))

(defun scala/smart-space ()
  "Insert a space, performing extra padding inside lists."
  (interactive)
  (cond
   ((and (scala/after-subexpr-opening?) (scala/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

(defun scala/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (super-smart-ops--run-with-modification-hooks
   (cond
    ((and (scala/after-subexpr-opening?)
          (scala/before-subexp-closing?)
          (thing-at-point-looking-at (rx (+ space))))
     (delete-horizontal-space))

    (t
     (or (super-smart-ops-delete-last-op)
         (call-interactively 'sp-backward-delete-char))))))


;;; Insert extends forms

(defun scala/insert-extends ()
  "Insert an extends form for the trait or class at point."
  (interactive)
  (save-match-data
    (scala/skip-toplevel-keyword-at-point)
    (-if-let (pos (scala/backward-class-decl))
        (progn
          (let* ((col (scala/column-at-pos (or (scala/start-of-extensions) (scala/end-of-extensions))))
                 (exts (scala/format-extensions-list col (scala/extensions-for-class))))
            (scala/delete-class-extensions)
            (goto-char (scala/end-of-extensions))
            (just-one-space)
            (insert exts)
            (evil-insert-state)
            (save-excursion
              (insert " "))))
      (user-error "No class or trait at point"))))

(defun scala/column-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun scala/backward-class-decl ()
  "Move to the last class or trait before point."
  (goto-char (line-end-position))
  (search-backward-regexp (rx (? "case" (+ space)) (or "class" "trait" "object")) nil t))

(defun scala/extensions-for-class ()
  (save-excursion
    (goto-char (line-beginning-position))
    (-when-let* ((exts-start (scala/start-of-extensions))
                 (exts-end (or (scala/end-of-extensions) (point-max)))
                 (span (buffer-substring exts-start exts-end)))
      (->> (s-split (rx (or "extends" "with")) span)
           (-map 's-trim)
           (-remove 's-blank?)))))

(defun scala/format-extensions-list (start-col extensions)
  "Combine extensions, splitting them over lines if the list becomes long."
  (if extensions
      (let* ((str (s-join " with " extensions))
             (width (+ start-col (length str)))
             (long-line? (<= fill-column width))
             (ext-str (if long-line? (s-replace "with" "\n    with" str) str)))
        (concat "extends " ext-str (when long-line? "\n ") " with "))
    "extends "))

(defvar scala/top-level-keywords-re
  (rx-to-string `(or "trait" "type" "private" "public" "package" "val" "var"
                     (and (? "case" (+ space))
                          (or "class" "object")))))

(defun scala/delete-class-extensions ()
  (save-excursion
    (goto-char (line-beginning-position))
    (-when-let* ((exts-start (scala/start-of-extensions))
                 (exts-end (or (scala/end-of-extensions) (point-max))))
      (delete-region exts-start exts-end))))

(defun scala/start-of-extensions ()
  (save-excursion
    (when (search-forward "extends" (line-end-position) t)
      (match-beginning 0))))

(defun scala/end-of-extensions ()
  (save-excursion
    (scala/skip-toplevel-keyword-at-point)

    (let* ((next-case       (scala/find-next "case"))
           (next-brace      (scala/find-next "{"))
           (next-empty-line (scala/find-next "\n\n"))
           (next-decl       (scala/end-of-toplevel-decl)))
      (-min (-non-nil (list next-case next-brace next-empty-line next-decl (point-max)))))))

(defun scala/find-next (str)
  (save-excursion
    (when (search-forward str nil t) (match-beginning 0))))

(defun scala/skip-toplevel-keyword-at-point ()
  (while (and (thing-at-point-looking-at scala/top-level-keywords-re)
              (not (eobp)))
    (search-forward-regexp (rx (* space) (+ word) (* space)) nil t)))

(defun scala/end-of-toplevel-decl ()
  (save-excursion
    (goto-char (line-end-position))
    (cond ((search-forward-regexp scala/top-level-keywords-re nil t)
           (goto-char (match-beginning 0))

           (forward-line -1)
           (goto-char (line-end-position))

           (while (and (not (bobp))
                       (s-blank? (current-line)))
             (forward-line -1)
             (goto-char (line-end-position))))

          (t
           (goto-char (line-end-position))))

    (point)))


;;; File template

(defun scala/package-for-current-file ()
  (-if-let* ((root (and (buffer-file-name) (projectile-project-p)))
             (pkg-id (scala/filepath-to-package-name root (buffer-file-name))))
      (cond
       ((s-blank? pkg-id) "")
       ((s-matches? (rx (or "Test" "IntTest" "Spec" "Prop") (? "s") eos)
                    (f-no-ext (f-filename (buffer-file-name))))
        "")
       (t
        (format "package %s\n\n" pkg-id)
        ))
    ""))

(defun scala/filepath-to-package-name (root file-name)
  (->> (f-dirname file-name)
       (s-chop-prefix root)
       f-split
       nreverse
       (--take-while (not (or (equal "src" it) (equal "scala" it))))
       nreverse
       (s-join ".")))


;;; SBT

(defun scala/tests-watch ()
  (interactive)
  (sbt-command "~test"))

(defun scala/test-only-watch ()
  (interactive)
  (let* ((impl-class
          (or (ensime-top-level-class-closest-to-point)
              (return (message "Could not find top-level class"))))
         (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
         (command (concat "~test-only  " cleaned-class)))
    (sbt-command command)))

(defun sbt ()
  (interactive)
  (require 'sbt-mode)
  (let ((default-directory (or (sbt:find-root)
                               (read-directory-name "Project root: " nil nil t))))
    (sbt-start)))


;;; Ensime utils

(defun sbt-gen-ensime (dir)
  (interactive (list
                (let ((dflt (or (locate-dominating-file default-directory ".ensime") (projectile-project-p))))
                  (read-directory-name "Directory: " nil nil t dflt))))

  (let ((default-directory (f-slash dir)))
    (message "Initialising Ensime at %s..." default-directory)
    (let* ((bufname (format "*sbt gen-ensime [%s]*" (f-filename dir)))
           (proc (start-process "gen-ensime" bufname "sbt" "gen-ensime"))

           (kill-process-buffer
            (lambda ()
              (when (get-buffer bufname)
                (-when-let (windows (--filter (equal (get-buffer bufname)
                                                     (window-buffer it))
                                              (window-list)))
                  (-each windows 'delete-window))
                (kill-buffer bufname))))

           (kill-proc-buffer
            (lambda (_ status)
              (when (s-matches? "finished" status)
                (ignore-errors
                  (funcall kill-process-buffer))
                (message "Ensime successfully initialised"))))
           )
      (set-process-sentinel proc kill-proc-buffer))))


;;; Test switching

(defun scala/impl-file-for-test-file (path)
  (--> (f-no-ext path)
       (s-replace-all `(("/test/" . "/main/")
                        ("Tests" . "")
                        ("Test" . "")
                        ("Specs" . "")
                        ("Spec" . "")
                        ("Props" . "")
                        ("Prop" . ""))
                      it)
       (concat it ".scala")))



(defadvice ensime-goto-impl (around display-buffer-nicely activate)
  (let ((impl-file-name (scala/impl-file-for-test-file (buffer-file-name))))
    (noflet ((ensime-goto-source-location
              (arg)
              (if arg (funcall this-fn arg) (find-file impl-file-name))))

      (save-window-excursion ad-do-it)
      (pop-to-buffer (find-file-noselect impl-file-name)))))

(defadvice ensime-goto-test (around pop-to-window activate)
  (let (buf)
    (save-window-excursion
      ad-do-it
      (setq buf (current-buffer)))
    (pop-to-buffer buf)))

;; Tweak behaviour of sbt:find-root to search for build.sbt

(after 'sbt-mode-project
  (defun sbt:find-root ()
    "Starting from the current default-directory, find the top-most
parent directory that is an sbt root. An sbt root directory is
identified by the following rules:

  - a directory containing a 'project/build.properties' in it.

  - a directory that contains a file matching one of the patterns
    '*.sbt' or 'project/*.scala' file in it.

The first rule is applied first and the second is used only if it
fails to find the sbt root."
    (or sbt:buffer-project-root
        (let ((root (or
                     (locate-dominating-file default-directory "build.sbt")
                     (sbt:find-root-impl "project/build.properties")
                     (sbt:find-root-impl
                      (lambda (dir)
                        (or (directory-files dir nil ".+\\.sbt$")
                            (and (file-exists-p (concat dir "project"))
                                 (directory-files (concat dir "project") nil ".+\\.scala$"))))))))
          (when root
            (setq-local sbt:buffer-project-root root))))))
