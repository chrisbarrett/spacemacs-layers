;; Leader commands

(defun org/goto-diary ()
  (interactive)
  (find-file org-agenda-diary-file))

(defun org/goto-notes ()
  (interactive)
  (find-file org-default-notes-file))

(defun org/goto-work ()
  (interactive)
  (find-file org-work-file))

(defun org/todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))

(defun org/tags-list ()
  "Show all tagged items."
  (interactive)
  (org-tags-view nil))

;; Project management

(defun org/project? ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun org/task? ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;;; Work

(defun org/refresh-agenda-when-toggling-work ()
  "Refresh the agenda when toggling between work states."
  (when (derived-mode-p 'org-agenda-mode)
    (org/agenda-dwim)))

;;; Tidy on save

(defun org/tidy-org-buffer ()
  "Perform cosmetic fixes to the current org buffer."
  (save-restriction
    (org-table-map-tables 'org-table-align 'quiet)
    ;; Realign tags.
    (org-set-tags 4 t)
    ;; Remove empty properties drawers.
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp ":PROPERTIES:" nil t)
        (save-excursion
          (org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0)))))))

;;; Tree editing

(defun org-narrow-to-subtree-content ()
  "Narrow to the content of the subtree.  Excludes the heading line."
  (widen)
  (unless (org-at-heading-p) (org-back-to-heading))
  (org-narrow-to-subtree)
  (forward-line)
  (narrow-to-region (line-beginning-position) (point-max)))

(defun org-subtree-content ()
  "Return the content of the subtree at point as a string."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-write-subtree-content (dest)
  "Write the contents of the subtree at point to a file at DEST."
  (interactive (list (ido-read-file-name "Write subtree to: " nil nil nil ".org")))
  (f-write-text (org-subtree-content) 'utf-8 dest)
  (when (called-interactively-p nil)
    (message "Subtree written to %s" dest)))

(defun org-copy-subtree-to ()
  "Create a duplicate of the current subtree at the given heading."
  (interactive "*")
  (atomic-change-group
    (org-copy-subtree)
    (org-clone-subtree-with-time-shift 1 '(16))
    (call-interactively 'org-refile)))

;;; Cascade TODO state changes.

(defun org/set-next-todo-state ()
  "When marking a todo to DONE, set the next TODO as NEXT.
Do not change habits, scheduled items or repeating todos."
  (when (equal org-state "DONE")
    (save-excursion
      (when (and (ignore-errors (outline-forward-same-level 1) t)
                 (equal (org-get-todo-state) "TODO"))
        (unless (or (org-is-habit-p)
                    (org-entry-get (point) "STYLE")
                    (org-entry-get (point) "LAST_REPEAT")
                    (org-get-scheduled-time (point)))
          (org-todo "NEXT"))))))

(defun org/children-done-parent-done (n-done n-todo)
  "Mark the parent task as done when all children are completed."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (zerop n-todo) "DONE" "TODO"))))

(defun org/mark-next-parent-tasks-todo ()
  "Visit each parent task and change state to TODO."
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (-contains? '("NEXT" "WAITING" "MAYBE")
                            (nth 2 (org-heading-components)))
            (org-todo "TODO")))))))


;;; Custom keyboard commands

(defun org/ctrl-c-ctrl-k (&optional n)
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive "p")
  (if (s-starts-with? "*Org" (buffer-name))
      (org-kill-note-or-show-branches)
    (org-cut-subtree n)))

(defun org/ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading' dep. on context."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-todo-heading))))

(defun org/agenda-dwim ()
  "Show the work agenda view if at work, otherwise the standard agenda."
  (interactive)
  (if (true? org-work--at-work?)
      (org-agenda current-prefix-arg "w")
    (org-agenda current-prefix-arg "A"))
  (delete-other-windows))


;;; Diary utils

(defvar date nil
  "Dynamic var bound to current date by calendaring functions.")

(defun calendar-nearest-to (target-dayname target-day target-month)
  "Non-nil if the current date is a certain weekday close to an anniversary.

TARGET-DAYNAME is the day of the week that we want to match,
 while TARGET-DAY and TARGET-MONTH are the anniversary."
  (let* ((dayname (calendar-day-of-week date))
         (target-date (list target-month target-day (calendar-extract-year date)))
         (days-diff (abs (- (calendar-day-number date)
                            (calendar-day-number target-date)))))
    (and (= dayname target-dayname)
         (< days-diff 4))))

(defun calendar-mondayised (target-day target-month)
  "Given anniversary with DAY and MONTH, return non-nil if:

- the given date is a weekday, or

- it is the Monday after the given date if it falls on a weekend."
  (if (memq (calendar-day-of-week date) '(6 0)) ; Sat or Sun
      (calendar-nearest-to 1 target-day target-month)

    (let ((m (calendar-extract-month date))
          (d (calendar-extract-day date)))
      (and (equal d target-day)
           (equal m target-month)))) )

(defun diary-limited-cyclic (recurrences interval m d y)
  "For use in emacs diary. Cyclic item with limited number of recurrences.
Occurs every INTERVAL days, starting on YYYY-MM-DD, for a total of
RECURRENCES occasions."
  (let ((startdate (calendar-absolute-from-gregorian (list m d y)))
        (today (calendar-absolute-from-gregorian date)))
    (and (not (cl-minusp (- today startdate)))
         (zerop (% (- today startdate) interval))
         (< (floor (- today startdate) interval) recurrences))))

(cl-defun org/format-class-sexpr ((s1 m1 h1 d1 m1 y1 . _)
                                  (s2 m2 h2 d2 m2 y2 . _)
                                  desc)
  "Parse dates into an org-class s-expression."
  (let* ((time (unless (or (zerop m1) (zerop h1))
                 (format " %.2i:%.2i %s" h1 m1 desc)))
         (date-range (list (list y1 m1 d1) (list y2 m2 d2)))
         (date-cols (-map (C
                           (~ s-pad-right 12 " ")
                           (~ s-join " ")
                           (~ -map (C (~ s-pad-left 2 " ")
                                      'number-to-string)))
                          date-range))
         (day-of-week (number-to-string (calendar-day-of-week (list m1 d1 y1)))))
    (concat "<%%(org-class   "
            (s-join " "  (-concat date-cols (list day-of-week)))
            ")>" time)))

(defun org-read-class ()
  "Read a class diary sexp with a description.
The starting day is taken to be the weekday on which the event will repeat."
  (let ((desc (read-string "Description: ")))
    (org/format-class-sexpr
     (org-parse-time-string (org-read-date nil nil nil "Start date: "))
     (org-parse-time-string (org-read-date nil nil nil "End date: "))
     desc)))

(defun org-insert-class ()
  "Read and insert a class diary sexp at point."
  (interactive "*")
  (insert (org-read-class)))

(defun calendar-easter-date (year)
  "Calculate the date for Easter Sunday in YEAR. Returns the date in the
Gregorian calendar, ie (MM DD YY) format."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(defun calendar-days-from-easter ()
  "When used in a diary sexp, this function will calculate how many days
are between the current date (DATE) and Easter Sunday."
  (- (calendar-absolute-from-gregorian date)
     (calendar-easter-date (calendar-extract-year date))))

(defun org/diary-update-appt-on-save ()
  (save-restriction
    (save-window-excursion
      (org-agenda-to-appt t)
      (appt-check 'force))))


;;; Archiving

(defun org/archive-done-tasks ()
  (interactive)
  (atomic-change-group
    (org-map-entries (lambda ()
                       ;; Ensure point does not move past the next item to
                       ;; archive.
                       (setq org-map-continue-from (point))
                       (org-archive-subtree))
                     "/DONE|PAID|VOID|CANCELLED" 'tree)))


;;; Tables

(defun org/recalculate-whole-table ()
  "Recalculate the current table using `org-table-recalculate'."
  (interactive "*")
  (when (org-at-table-p)
    (let ((before (buffer-substring (org-table-begin) (org-table-end))))
      (org-table-recalculate '(16))
      (let ((after (buffer-substring (org-table-begin) (org-table-end))))
        (if (equal before after)
            (message "Table up-to-date")
          (message "Table updated"))))))


;;; Clocks

(defun org/remove-empty-clock-drawers ()
  "Remove empty clock drawers at point."
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(defun org/clock-in-to-next-state (_kw)
  "Move a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO."
  (unless (true? org-capture-mode)
    (cond
     ((and (-contains? '("TODO") (org-get-todo-state))
           (org/task?))
      "NEXT")
     ((and (-contains? '("NEXT") (org-get-todo-state))
           (org/project?))
      "TODO"))))


;;; Crypt

(defun org/looking-at-pgp-section? ()
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((heading-point (point))
            (heading-was-invisible-p
             (save-excursion
               (outline-end-of-heading)
               (outline-invisible-p))))
        (forward-line)
        (looking-at "-----BEGIN PGP MESSAGE-----")))))

(defun org/decrypt-entry ()
  (when (org/looking-at-pgp-section?)
    (org-decrypt-entry)
    t))


;;; Export

(defun org-export/koma-letter-at-subtree (dest)
  "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name.
The PDF will be created at DEST."
  (interactive
   (list (if current-prefix-arg
             (ido-read-file-name "Destination: " nil nil nil ".pdf")
           (concat (f-no-ext (buffer-file-name)) ".pdf"))))

  (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
    (org-write-subtree-content tmpfile)
    (with-current-buffer (find-file-noselect tmpfile)
      (unwind-protect
          (-if-let (exported (org-koma-letter-export-to-pdf))
              (f-move exported dest)
            (error "Export failed"))
        (kill-buffer)))
    (async-shell-command (format "open %s" (shell-quote-argument dest)))
    (message "opening %s..." dest)))

(defun org/C-c-C-c-export-koma-letter ()
  "Export the koma letter at point."
  (when (ignore-errors
          (s-matches? (rx "latex_class:" (* space) "koma")
                      (org-subtree-content)))
    (call-interactively 'org-export-koma-letter-at-subtree)
    'export-koma-letter))

;;; Config support

(defun org/add-local-hooks ()
  "Set buffer-local hooks for orgmode."
  (add-hook 'after-save-hook 'org/diary-update-appt-on-save nil t)
  (add-hook 'org-after-todo-state-change-hook 'org/mark-next-parent-tasks-todo nil t)
  (add-hook 'org-clock-in-hook 'org/mark-next-parent-tasks-todo nil t)
  (add-hook 'before-save-hook 'org/tidy-org-buffer nil t))

(defun org/exclude-tasks-on-hold (tag)
  (and (equal tag "hold") (concat "-" tag)))
