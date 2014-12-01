(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")
(require 'cl-lib)
(require 'dash)
(require 's)


;;; Dash extensions

(defun -true-fn (&rest _)
  "Always return t."
  t)

(defun -nil-fn (&rest _)
  "Always return nil."
  nil)

(defun -listify (x)
  "Wrap X in a list if it is not a list."
  (if (listp x)
      x
    (list x)))

(defun -uniq-by (selector-fn list)
  "Remove duplicates in the given sequence using a function.

- SELECTOR-FN takes the current element and returns the item to compare.

- LIST is the sequence to transform."
  ;; Cache the items compared using selector-fn for later comparisons. This
  ;; alleviates the need for an additional traversal.
  (let (transformed)
    (--reduce-r-from
     (let ((cur (funcall selector-fn it)))
       (if (-contains? transformed cur)
           acc
         (push cur transformed)
         (cons it acc)))
     nil
     list)))

(defun -non-null (list)
  "Return the non-nil elements in LIST."
  (-keep 'identity list))


;;; S extensions

(defun s-alnum-only (s)
  "Remove non-alphanumeric characters from S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (search-forward-regexp (rx (not alnum)) nil t)
      (replace-match ""))
    (buffer-string)))

(defun s-unlines (&rest strs)
  "Join STRS with newlines."
  (s-join "\n" strs))

(defmacro s-lex-cat (&rest format-strs)
  "Concatenate FORMAT-STRS then pass them to `s-lex-format'."
  `(s-lex-format ,(apply 'concat format-strs)))

(defmacro s-with-temp-buffer (&rest body)
  "Evaluate BODY in a temporary buffer and return the buffer string."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     ,@body
     (buffer-string)))

(defalias 's-no-props 'substring-no-properties)

(defun s-split-sexps (str)
  "Split STR by sexp boundaries."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; Collect sexps in buffer.
    (let (acc (pt (point-min)))
      (until (eobp)
        (forward-sexp)
        (setq acc (cons (s-trim (buffer-substring pt (point)))
                        acc))
        (setq pt (point)))

      (-remove 's-blank? (nreverse acc)))))


;;; Core forms

(defmacro until (test &rest body)
  "If TEST yields nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns non-nil."
  (declare (indent 1))
  `(while (not ,test)
     ,@body))

(defmacro lambda+ (arglist &rest body)
  "A lambda function supporting argument destructuring.

ARGLIST is a full Common Lisp arglist.  Its bindings are availabe
in BODY.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  `(lambda (&rest args)
     (cl-destructuring-bind ,arglist args
       ,@body)))

(defmacro after (features &rest body)
  "Like `eval-after-load' - once all FEATURES are loaded, execute the BODY.
FEATURES may be a symbol or list of symbols."
  (declare (indent 1))
  ;; Wrap body in a descending list of `eval-after-load' forms.
  ;; The last form is eval'd to remove its quote.
  (eval (->> (-listify (eval features))
          (--map `(eval-after-load ',it))
          (--reduce-from `'(,@it ,acc)
                         `'(progn ,@body)))))

(defmacro command (&rest body)
  "Declare an `interactive' command with BODY forms."
  `(lambda (&optional _arg &rest _args)
     (interactive)
     ,@body))

(defmacro true? (sym)
  "Test whether SYM is bound and non-nil."
  `(and (boundp ',sym) (eval ',sym)))


;;; Useful functions

(defun current-region (&optional no-properties)
  "Return the current active region, or nil if there is no region active.
If NO-PROPERTIES is non-nil, return the region without text properties."
  (when (region-active-p)
    (funcall (if no-properties 'buffer-substring-no-properties 'buffer-substring)
             (region-beginning)
             (region-end))))

(cl-defun current-line (&optional (move-n-lines 0))
  "Return the line at point, or another line relative to this line.
MOVE-N-LINES is an integer that will return a line forward if
positive or backward if negative."
  (save-excursion
    (forward-line move-n-lines)
    (buffer-substring (line-beginning-position) (line-end-position))))

(cl-defun collapse-vertical-whitespace (&optional (to-n-lines 1))
  "Collapse blank lines around point.
TO-N-LINES is the number of blank lines to insert afterwards."
  (interactive "*nCollapse to N blanks: ")
  (save-excursion
    ;; Delete blank lines.
    (search-backward-regexp (rx (not (any space "\n"))) nil t)
    (forward-line 1)
    (while (s-matches? (rx bol (* space) eol) (current-line))
      (forward-line)
      (join-line))
    ;; Open a user-specified number of blanks.
    (open-line to-n-lines)))

(defun filter-atoms (predicate)
  "Return the elements of the default obarray that match PREDICATE."
  (let (acc)
    (mapatoms (lambda (atom)
                (when (funcall predicate atom)
                  (push atom acc))))
    acc))


;;; Buffer utils

(cl-defmacro --filter-buffers (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-filter-buffers'"
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(cl-defmacro --map-buffers (form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-map-buffers'"
  `(--map (with-current-buffer it ,form) ,bufs))

(cl-defmacro --first-buffer (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-first-buffer'"
  `(--first (with-current-buffer it ,pred-form) ,bufs))

(defalias '-first-window 'get-window-with-predicate)

(defmacro --first-window (pred-form)
  "Anaphoric form of `-first-window'.
Find the first window where PRED-FORM is not nil."
  `(-first-window (lambda (it) ,pred-form)))


;;; Buffer management

(defun core/clean-buffers ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (-each (--filter-buffers
          (not (or (-contains? core/kill-buffer-ignored-list (buffer-name it))
                   (get-buffer-process it))))
    'kill-buffer))


(defvar core/kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*"
    "*shell*" "*eshell*" "*ansi-term*"
    "diary.org" "notes.org"))

(defun core/kill-this-buffer ()
  "Kill the current buffer.
If this buffer is a member of `core/kill-buffer-ignored-list', bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) core/kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))


(defun core/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defalias 'rfb 'core/rename-file-buffer)
(defalias 'rbf 'core/rename-file-buffer)


(defun core/delete-file-and-buffer ()
  "Delete a file and its associated buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defalias 'dfb 'core/delete-file-and-buffer)
(defalias 'dbf 'core/delete-file-and-buffer)


;;; Line transposition

(defun core/move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun core/move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))


;;; Misc interactive commands

(defun remove-line-breaks ()
  "Remove line endings in a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))

(defun core/exit-emacs ()
  (interactive)
  (when (yes-or-no-p "Kill Emacs? ")
    (if (daemonp)
        (server-save-buffers-kill-terminal nil)
      (save-buffers-kill-emacs))))

(defun core/warn-exit-emacs-rebound ()
  (interactive)
  (user-error "Type <C-c k k> to exit Emacs"))

(defun core/comma-then-space ()
  (interactive)
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (if in-string-or-comment?
        (insert ",")
      (save-restriction
        (narrow-to-region (line-beginning-position) (point))
        (atomic-change-group
          (when (thing-at-point-looking-at (rx (not space) (* space)))
            (delete-horizontal-space t))
          (insert-char ?\,)
          (just-one-space))))))



;;; Indentation

(defun core/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (indent-for-tab-command)
        (forward-line)))))

(defun core/indent-dwim (&optional arg)
  "Perform a context-sensitive indentation action.
With prefix argument ARG, justify text."
  (interactive "P")
  (let ((in-string? (nth 8 (syntax-ppss))))
    (cond
     ((region-active-p)
      (indent-region (region-beginning) (region-end))
      (message "Indented region."))

     (in-string?
      (if (apply 'derived-mode-p core/lisp-modes)
          (lisp-fill-paragraph arg)
        (or (fill-comment-paragraph)
            (fill-paragraph arg)))
      (message "Filled paragraph."))

     (t
      (core/indent-buffer)
      (message "Indented buffer.")))))

(defun core/outdent ()
  "Remove indentation on the current line."
  (interactive "*")
  (save-excursion
    (goto-char (line-beginning-position))
    (delete-horizontal-space)))

;;; Elisp

(defun core/config-elisp-surround-pairs ()
  "Configure backtick pair for Elisp docstrings."
  (make-local-variable 'evil-surround-pairs-alist)
  (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))


;;; Compilation

(defun core/compile-autoclose (buf string)
  "Automatically close the compile window."
  (cond
   ;; Ignore if this isn't a normal compilation window.
   ((not (equal (buffer-name buf) "*compilation*")))

   ((not (s-contains? "finished" string))
    (message "Compilation exited abnormally: %s" string))

   ((s-contains? "warning"
                 (with-current-buffer buf (buffer-string))
                 t)
    (message "Compilation succeeded with warnings"))

   (t
    (ignore-errors
      (delete-window (get-buffer-window buf)))
    (message "Compilation succeeded"))))

(defun core/ansi-colourise-compilation ()
  (ansi-color-apply-on-region compilation-filter-start (point)))


;;; Font lock

(defun core/font-lock-replace-match (regex group replacement)
  "Return a font-lock replacement spec for.

REGEX surrounds the text to be replaced with a group.

GROUP is the number of the group.

REPLACEMENT is the string to substitute for the match in REGEX."
  (list regex
        `(0 (progn (compose-region (match-beginning ,group) (match-end ,group)
                                   ,replacement 'decompose-region)
                   nil))))
