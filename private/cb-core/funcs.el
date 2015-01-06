;; -*- lexical-binding: t; -*-

;;; Code:

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")
(require 'cl-lib)
(require 's)
(require 'dash)



(defun -listify (x)
  "Wrap X in a list if it is not a list."
  (if (listp x)
      x
    (list x)))

(defun s-unlines (&rest strs)
  "Join STRS with newlines."
  (s-join "\n" strs))


;;; Core forms

(defmacro until (test &rest body)
  "If TEST yields nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns non-nil."
  (declare (indent 1))
  `(while (not ,test)
     ,@body))


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

(defun filter-atoms (predicate)
  "Return the elements of the default obarray that match PREDICATE."
  (let (acc)
    (mapatoms (lambda (atom)
                (when (funcall predicate atom)
                  (push atom acc))))
    acc))

(cl-defmacro --filter-buffers (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-filter-buffers'"
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(defun core/read-string-with-default (prompt default &optional initial-input history)
  "Read a string from the user with a default value added to the prompt."
  (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
               initial-input history default))

(defun core/open-line-below-current-indentation ()
  "Open a new line below at the current indent level."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

(defun core/in-string-or-comment? ()
  "Non-nil if point is at a string or comment."
  (nth 8 (syntax-ppss)))


;;; Buffer management

(defvar core/kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*"
    "*shell*" "*eshell*" "*ansi-term*"
    "diary.org" "notes.org" "*spacemacs*"))

(defun core/clean-buffers ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (let* ((other-buffers (-difference (buffer-list) (list (current-buffer))))
         (buffer-ignored-or-live (lambda (b) (not (or (-contains? core/kill-buffer-ignored-list (buffer-name b))
                                                      (get-buffer-process b))))))
    (-each (--filter-buffers 'buffer-ignored-or-live other-buffers)
      'kill-buffer)))

(defun core/kill-this-buffer ()
  "Kill the current buffer.
If this buffer is a member of `core/kill-buffer-ignored-list', bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) core/kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun core/move-file (buffer to-dir)
  "Move BUFFER's corresponding file to DEST."
  (interactive (list (current-buffer) (read-directory-name "Move to: ")))
  (let ((dest (f-join to-dir (f-filename (core/buffer-file-name-assert-exists buffer)))))
    (core/rename-file-and-buffer buffer dest)))

(defun core/rename-file-and-buffer (buffer dest-path)
  "Rename the current buffer and file it is visiting."
  (interactive (let ((cur (core/buffer-file-name-assert-exists)))
                 (list (current-buffer) (read-file-name "New name: " cur))))
  (let ((src (core/buffer-file-name-assert-exists buffer)))
    (or (core/try-move-file-with-vc src dest-path)
        (core/try-rename-file src dest-path))
    (message "File '%s' moved to '%s'" (f-short (f-filename src)) (f-short dest-path))))

(defun core/buffer-file-name-assert-exists (&optional buf)
  (let ((cur (buffer-file-name buf)))
    (if (not (and cur (f-exists? cur)))
        (error "Buffer is not visiting a file!")
      cur)))

(defun core/try-move-file-with-vc (src dest)
  (condition-case err
      (when (vc-backend src)
        (vc-rename-file src dest)
        t)
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((s-matches? "New file already exists" msg) nil)
        ((s-matches? "Please update files" msg)
         (unless (y-or-n-p "VC cannot track this change automatically. Continue?")
           (error msg)))
        (t
         (error msg)))))))

(defun core/try-rename-file (src dest)
  (when (and (f-exists? dest) (not (y-or-n-p "File exists. Overwrite?")))
    (user-error "Aborted"))
  (rename-file src dest t)
  (-when-let (buf (get-file-buffer src))
    (with-current-buffer buf
      (rename-buffer dest)
      (set-visited-file-name dest)
      (set-buffer-modified-p nil))))

(defun core/delete-file-and-buffer (filename buffer)
  "Delete a file and its associated buffer."
  (interactive (list (buffer-file-name) (current-buffer)))
  (when (and (f-exists? filename) (yes-or-no-p "Are you sure you want to remove this file? "))
    (if (vc-backend filename)
        (vc-delete-file filename)
      (delete-file filename)))
  (ignore-errors (kill-buffer buffer))
  (message "File '%s' successfully removed" (f-short filename)))


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

(defvar core/indent-commands-alist
  nil
  "Alist of commands to run to indent the buffer, indexed by major-mode")

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

     ((assoc major-mode core/indent-commands-alist)
      (funcall (cdr (assoc major-mode core/indent-commands-alist)))
      (message "Formatted buffer."))

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


;;; File template

(defun core/template-org-skeleton-title (filename)
  "Format the title to use for the given FILENAME."
  (->> (f-filename (f-no-ext filename))
       s-split-words
       (-map 's-capitalize)
       (s-join " ")))

(defun core/reset-buffer-undo-history ()
  "Set file-local vars for expanded file templates."
  (setq-local buffer-undo-list nil)
  (setq-local buffer-undo-tree nil))


;;; Global insertion commands

(defun core/insert-timestamp ()
  "Read a timestamp from the user and insert it at point."
  (interactive)
  (let ((time (current-time)))
    (helm :prompt "Timestamp: "
          :buffer "*Helm Timestamp*"
          :sources
          `(((name . "Dates")
             (candidates . ,(list
                             (format-time-string "%d-%m-%y" time)
                             (format-time-string "%d-%m-%Y" time)
                             (format-time-string "%d-%m-%Y %H:%M" time)
                             (format-time-string "%d-%m-%Y %I:%M %p" time)))
             (action . insert)
             (volatile))

            ((name . "Times")
             (candidates . ,(list
                             (format-time-string "%X" time)
                             (format-time-string "%I:%M %p" time)
                             (format-time-string "%I:%M:%S %p" time)))
             (action . insert)
             (volatile))

            ((name . "Special")
             (candidates . ,(list
                             (format-time-string "%d %B, %Y" time)
                             (format-time-string "%Y-%m-%dT%H%M%S%z")))
             (action . insert)
             (volatile))))))

(defun core//filename->interpreter (filename)
  (cdr
   (assoc (file-name-extension filename)
          '(("el" . "emacs")
            ("hs" . "runhaskell")
            ("py" . "python")
            ("rb" . "ruby")
            ("sh" . "bash")))))

(defun core/insert-shebang (cmd)
  "Insert a shebang line at the top of the current buffer.
Prompt for a command CMD if one cannot be guessed."
  (interactive
   (list (or (core//filename->interpreter buffer-file-name)
             (read-string "Command name: " nil t))))
  (save-excursion
    (goto-char (point-min))
    (open-line 2)
    (insert (concat "#!/usr/bin/env " cmd))))

(defun core/insert-variable-value (variable)
  "Insert the value of VARIABLE at point."
  (interactive
   (list
    (intern
     (ido-completing-read
      "Variable: "
      (-map 'symbol-name
            (filter-atoms (-orfn 'custom-variable-p 'special-variable-p)))))))
  (insert (pp-to-string (eval variable))))

(defun core/make-uuid ()
  "Generate a UUID using the uuid utility."
  (s-trim-right (shell-command-to-string "uuidgen")))

(defun core/insert-uuid ()
  "Insert a GUID at point."
  (interactive "*")
  (insert (core/make-uuid)))

(defalias 'insert-guid 'core/insert-uuid)

(defun core/insert-lorem-ipsum (n-paragraphs paragraph-length)
  "Insert N-PARAGRAPHS of lorem ipsum text into the current buffer.
PARAGRAPH-LENGTH is one of short, medium, long or verylong."
  (interactive
   (list (read-number "Number of paragraphs: " 3)
         (ido-completing-read "Paragraph length: "
                              '("short" "medium" "long" "verylong"))))
  (let ((url (format "http://loripsum.net/api/%s/%s/plaintext"
                     n-paragraphs paragraph-length)))
    (insert (with-current-buffer (url-retrieve-synchronously url)
              ;; Skip HTTP header.
              (goto-char (point-min))
              (search-forward "\n\n")
              (s-trim (buffer-substring (point) (point-max)))))))


;;; Create indirect buffer from region.

(defvar-local indirect-mode-name nil
  "Mode to set for indirect buffers.")

(defun indirect-region (start end mode)
  "Edit the current region in another buffer.
Edit from START to END using MODE."
  (interactive
   (list (region-beginning)
         (region-end)
         (intern (completing-read
                  "Mode: "
                  (--map (list (symbol-name it))
                         (apropos-internal "-mode$" 'commandp))
                  nil t indirect-mode-name))))

  (setq indirect-mode-name (symbol-name mode))
  (let ((buffer-name (generate-new-buffer-name "*indirect*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))


;;; Super smart ops

(defun core/insert-smart-op-no-leading-space (op)
  "Insert OP without any preceding padding."
  (super-smart-ops-insert op)
  (save-excursion
    (search-backward op)
    (unless (s-matches? (rx bol (* space) eol)
                        (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space))))

(defun core/comma-then-space ()
  "Insert a comma smart op, removing any preceding padding."
  (interactive)
  (core/insert-smart-op-no-leading-space ","))

(defun core/semicolon-then-space ()
  "Insert a colon, with context-sensitive formatting."
  (interactive)
  (core/insert-smart-op-no-leading-space ";"))
