;;; funcs.el --- Helper functions for cb-scala layer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'noflet)
(require 's)
(require 'thingatpt)

(autoload 'cb-buffers-filtera "cb-buffers")
(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")
(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'ensime "ensime")
(autoload 'ensime--get-name "ensime-config")
(autoload 'ensime-inf-load-file "ensime-inf")
(autoload 'ensime-inf-run-scala "ensime-inf")
(autoload 'ensime-top-level-class-closest-to-point "ensime-goto-testfile")
(autoload 'ensime-config-find "ensime-config")
(autoload 'ensime-config-load "ensime-config")
(autoload 'evil-insert-state "evil-states")
(autoload 'sbt-command "sbt-mode")
(autoload 'sbt:find-root-impl "sbt-mode-project")
(autoload 'projectile-project-p "projectile")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'yas-expand-snippet "yasnippet")

;;; Interactive

(defun scala/switch-to-src ()
  "Switch back to the last scala source file."
  (interactive)
  (-when-let (buf (car (cb-buffers-filtera (derived-mode-p 'scala-mode))))
    (pop-to-buffer buf)))

(defun scala/load-buffer (file)
  "Load FILE, starting an inferior scala if needed."
  (interactive (list (buffer-file-name)))
  (unless (get-buffer ensime-inf-buffer-name)
    (ensime-inf-run-scala))
  (ensime-inf-load-file file))

(defconst scala/unicode-mapping-alist
  '(("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")))

(defun scala/unicode-buffer ()
  "Rewrite symbols in buffer."
  (interactive)
  (when (derived-mode-p 'scala-mode)
    (--each scala/unicode-mapping-alist
      (-let* (((ascii . unicode) it)
              (match-ascii  (rx-to-string `(and space (group ,ascii) space)
                                          nil)))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp match-ascii nil t)
            (replace-match unicode nil t nil 1)))))))


;;; Smart editing commands

(defun scala/after-lambda-arrow? ()
  (s-matches? (rx (* space) (or "=>" "⇒") (* space) eos)
              (buffer-substring (line-beginning-position) (point))))

(defun scala/blank-up-to-curly? ()
  (s-matches? (rx bos (* space) "}") (buffer-substring (point) (line-end-position))))

(defun scala/brace-group-starts-with-case-expr? ()
  (when (sp-inside-curly-braces-with-content?)
    (-let [(&plist :beg beg :end end) (sp-get-enclosing-sexp)]
      (s-matches? (rx (* space) "case")
                  (buffer-substring (1+ beg) (1- end))))))

(defun scala/maybe-swing-down-lambda-body ()
  (when (search-backward-regexp (rx (or "=>" "⇒")) (line-beginning-position) t)
    (goto-char (match-end 0))
    (newline-and-indent)))

(defun scala--open-line-below-current-indentation ()
  "Open a new line below at the current indent level."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

(defun scala/ret (&optional arg)
  "Insert a newline with context-sensitive formatting."
  (interactive "P")
  (let ((sexp (sp-get-enclosing-sexp)))
    (cond
     ((scala/at-scaladoc?)
      (goto-char (line-end-position))
      (scala--open-line-below-current-indentation)
      (insert "* "))

     ((or arg (cb-buffers-in-string-or-comment?))
      (comment-indent-new-line)
      (just-one-space))

     ((sp-inside-curly-braces-blank-content? nil sexp)
      (sp-split-braced-expression-over-new-lines (rx ";") sexp))

     ((and (sp-inside-curly-braces-with-content? t sexp)
           (scala/blank-up-to-curly?)
           (scala/after-lambda-arrow?))
      (sp-split-braced-expression-over-new-lines (rx ";") sexp)
      (goto-char (line-end-position))
      (save-excursion
        (scala/maybe-swing-down-lambda-body))
      (newline-and-indent))

     ((and (sp-inside-curly-braces-with-content? t sexp)
           (scala/brace-group-starts-with-case-expr?))
      (sp-split-braced-expression-over-new-lines (rx ";") sexp)
      (cond
       ((scala/after-lambda-arrow?)
        (newline-and-indent))
       (t
        (goto-char (line-end-position))
        (scala/maybe-swing-down-lambda-body)))

      (goto-char (line-end-position)))

     ;; ((sp-inside-curly-braces-with-content? t)
     ;;  (sp-split-braced-expression-over-new-lines (rx ";"))
     ;;  (goto-char (line-end-position))
     ;;  (save-excursion
     ;;    (scala/maybe-swing-down-lambda-body)
     ;;    (goto-char (plist-get (sp-get-enclosing-sexp) :beg))
     ;;    (spacemacs/scala-join-line)))

     (t
      (sp-generic-prog-ret)))))

(defun scala/at-scaladoc? ()
  (s-matches? (rx bol (* space) (? "/") (+ "*")) (cb-buffers-current-line)))

(defun scala/at-case-class? ()
  (s-matches? (rx bol (* space) "case" (+ space) "class" eow) (cb-buffers-current-line)))

(defun scala/at-case-object? ()
  (s-matches? (rx bol (* space) "case" (+ space) "object" eow) (cb-buffers-current-line)))

(defun scala/at-abstract-sealed-class? ()
  (s-matches? (rx bol (* space) "abstract" (+ space) "sealed" (+ space) "class" eow) (cb-buffers-current-line)))

(defun scala/at-sealed-trait? ()
  (s-matches? (rx bol (* space) "sealed" (+ space) "trait" eow) (cb-buffers-current-line)))

(defun scala/meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- In match statements
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "var" eow) (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "var ${1:ident} = $0")
    (message "New var binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "private" (+ space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "private val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "protected" (+ space) (? "lazy" (+ space)) "val" eow) (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "protected val ${1:ident} = $0")
    (message "New val binding"))

   ;; Insert new case class.
   ((or (scala/at-case-class?) (scala/at-sealed-trait?) (scala/at-abstract-sealed-class?))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "case class ${1:Case}(${2:params...})")
    (message "New case class"))

   ;; Insert new case object.
   ((scala/at-case-object?)
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "case object ${1:Name}")
    (message "New case object"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "case") (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
    (yas-expand-snippet "case ${1:binding} => $0")
    (message "New data case"))

   ;; Insert new import statement
   ((s-matches? (rx bol (* space) "import" eow) (cb-buffers-current-line))
    (scala--open-line-below-current-indentation)
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
                       (s-blank? (cb-buffers-current-line)))
             (forward-line -1)
             (goto-char (line-end-position))))

          (t
           (goto-char (line-end-position))))

    (point)))


;;; SBT

(defun scala/tests-watch ()
  (interactive)
  (sbt-command "~test"))

(defun scala/test-only-watch ()
  (interactive)
  (let* ((impl-class
          (or (ensime-top-level-class-closest-to-point)
              (cl-return (message "Could not find top-level class"))))
         (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
         (command (concat "~test-only  " cleaned-class)))
    (sbt-command command)))

(autoload 'sbt-start "sbt")

(autoload 'sbt:find-root "sbt-mode")

(defun scala/sbt-for-dir (dir)
  (noflet ((sbt:find-root (&rest args) (f-abbrev dir)))
    (let ((buf (save-window-excursion (sbt-start))))
      (pop-to-buffer buf))))

(defun sbt (dir)
  (interactive (list (read-directory-name "Project root: " (sbt:find-root) nil t)))
  (scala/sbt-for-dir dir))

(defun scala/sbt-send-ret ()
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\n")
  (goto-char (point-max))
  (evil-insert-state))

(defun scala-sbt/bring (&optional new)
  "Display an sbt buffer, creating a new one if needed.
With prefix argument ARG, always create a new shell."
  (interactive "P")
  (cond
   (new
    (scala-sbt--new))
   ((derived-mode-p 'sbt-mode)
    (scala-sbt--hide))
   ((scala-sbt--buffer)
    (scala-sbt--show))
   (t
    (scala-sbt--new))))

(defun scala-sbt--hide ()
  (let ((reg (scala-sbt--mk-register-name)))
    (if (get-register reg)
        (or (ignore-errors (jump-to-register reg t) t)
            (bury-buffer))
      (bury-buffer)
      (when (< 1 (length (window-list)))
        (delete-window)))))

(defun scala-sbt--new ()
  (window-configuration-to-register (scala-sbt--mk-register-name))
  (save-window-excursion
    (-if-let (root (projectile-project-p))
        (scala/sbt-for-dir root)
      (call-interactively 'sbt)))
  (scala-sbt--show))

(defun scala-sbt--show ()
  (window-configuration-to-register (scala-sbt--mk-register-name))
  (pop-to-buffer (scala-sbt--buffer)))

(defun scala-sbt--buffer ()
  (let ((current-frame (scala-sbt--current-frame)))
    (--first (with-current-buffer it
               (and (derived-mode-p 'sbt-mode)
                    (s-matches? "sbt" (buffer-name it))
                    (equal current-frame (window-frame (get-buffer-window it)))))
             (buffer-list))))

(defun scala-sbt--current-frame ()
  (window-frame (get-buffer-window (current-buffer))))

(defun scala-sbt--mk-register-name ()
  (-let [(&alist 'window-id id) (frame-parameters (scala-sbt--current-frame))]
    (intern (format "scala-sbt-%s" id))))


;;; Ensime utils

(defun scala/maybe-project-root ()
  (or (locate-dominating-file default-directory ".ensime") (projectile-project-p)))

(defun scala/inf-ensime-buffer-name ()
  (-when-let* ((config-file (-if-let (root (scala/maybe-project-root))
                                (f-join root ".ensime")
                              (ensime-config-find)))
               (_ (f-exists? config-file))
               (config (let ((inhibit-redisplay t))
                         (ensime-config-load config-file)))
               (name (ensime--get-name config))
               (bufname (or (plist-get config :buffer)
                            (concat ensime-default-buffer-prefix name))))
    (format "*%s*" bufname)))

(defun scala/inf-ensime-buffer ()
  (-when-let (bufname (scala/inf-ensime-buffer-name))
    (get-buffer bufname)))

(defun sbt-gen-ensime (dir)
  (interactive (list
                (read-directory-name "Directory: " nil nil t (scala/maybe-project-root))))

  (let ((default-directory (f-slash dir)))
    (-when-let (buf (scala/inf-ensime-buffer))
      (message "Killing an existing Ensime server.")
      (kill-buffer buf))

    (message "Initialising Ensime at %s." default-directory)

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
                (scala/fix-ensime-file)
                (ignore-errors
                  (funcall kill-process-buffer))
                (ensime)
                (message "Ensime ready.")))))

      (set-process-sentinel proc kill-proc-buffer)
      (display-buffer bufname)
      (recenter-top-bottom -1)
      (redisplay))))

(defun scala/fix-ensime-file (&optional file)
  "Fix malformed scalariform settings in FILE."
  (interactive)
  (require 'ensime)
  (let* ((file (or file (ensime-config-find)))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (scala/fix-dot-ensime)
      (let ((modified? (buffer-modified-p)))
        (save-buffer 0)
        (if modified?
            (message "Fixed ensime file")
          (message "No changes were needed"))))
    (kill-buffer buf)))

(defun scala/fix-dot-ensime ()
  (let ((invalid-formatter-rx
         (rx ":alignSingleLineCaseStatements" (group ".") "maxArrowIndent")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp invalid-formatter-rx nil t)
        (replace-match "_" t t nil 1)))))


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

(with-eval-after-load 'sbt-mode-project
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

(defun cb-scala/turn-off-aggressive-indent ()
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode -1)))

;;; funcs.el ends here
