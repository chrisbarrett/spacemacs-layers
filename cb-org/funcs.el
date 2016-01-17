;;; funcs.el --- supporting functions for org config.
;;; Commentary:
;;; Code:

(require 'calendar)
(require 'dash)
(require 'f)
(require 's)

(autoload 'appt-check "appt")
(autoload 'org-agenda-filter-apply "org-agenda")
(autoload 'org-at-heading-p "org")
(autoload 'org-at-table-p "org")
(autoload 'org-back-to-heading "org")
(autoload 'org-clock-special-range "org-clock")
(autoload 'org-clone-subtree-with-time-shift "org")
(autoload 'org-content "org")
(autoload 'org-copy-subtree "org")
(autoload 'org-cut-subtree "org")
(autoload 'org-dblock-write:clocktable "org-clock")
(autoload 'org-find-exact-headline-in-buffer "org")
(autoload 'org-float-time "org-compat")
(autoload 'org-kill-note-or-show-branches "org")
(autoload 'org-narrow-to-subtree "org")
(autoload 'org-parse-time-string "org")
(autoload 'org-time-stamp-format "org")

;; Leader commands

(defun cb-org/goto-diary ()
  (interactive)
  (find-file org-agenda-diary-file))

(defun cb-org/goto-notes ()
  (interactive)
  (find-file org-default-notes-file))

(defun cb-org/goto-work ()
  (interactive)
  (find-file org-work-file))

(defun cb-org/todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))

(defun cb-org/tags-list ()
  "Show all tagged items."
  (interactive)
  (org-tags-view nil))


;;; Tree editing

(defun cb-org-narrow-to-subtree-content ()
  "Narrow to the content of the subtree.  Excludes the heading line."
  (widen)
  (unless (org-at-heading-p) (org-back-to-heading))
  (org-narrow-to-subtree)
  (forward-line)
  (narrow-to-region (line-beginning-position) (point-max)))

(defun cb-org-subtree-content ()
  "Return the content of the subtree at point as a string."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun cb-org-write-subtree-content (dest)
  "Write the contents of the subtree at point to a file at DEST."
  (interactive (list (ido-read-file-name "Write subtree to: " nil nil nil ".org")))
  (f-write-text (cb-org-subtree-content) 'utf-8 dest)
  (when (called-interactively-p nil)
    (message "Subtree written to %s" dest)))

(defun cb-org-copy-subtree-to ()
  "Create a duplicate of the current subtree at the given heading."
  (interactive "*")
  (atomic-change-group
    (org-copy-subtree)
    (org-clone-subtree-with-time-shift 1 '(16))
    (call-interactively 'org-refile)))


;;; Custom keyboard commands

(defun cb-org/ctrl-c-ctrl-k (&optional n)
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive "p")
  (if (s-starts-with? "*Org" (buffer-name))
      (org-kill-note-or-show-branches)
    (org-cut-subtree n)))

(defun cb-org/ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading'."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-todo-heading))))

(defun cb-org/agenda-dwim ()
  "Show the work agenda view if at work, otherwise the standard agenda."
  (interactive)
  (if (and (boundp 'org-work--at-work?) org-work--at-work?)
      (org-agenda current-prefix-arg "w")
    (org-agenda current-prefix-arg "A"))
  (delete-other-windows))


;;; Diary utils

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

(defun holiday-mondayised (year month day)
  "Return a mondayised holiday at YEAR for the holiday at MONTH, DAY."
  (let ((date (list month day year)))
    (cl-case (calendar-day-of-week date)
      ;; Weekday - use that date
      ((1 2 3 4 5) date)
      ;; Weekend - use following Monday.
      (t
       (calendar-nth-named-day 1 1 month year day)))))

(defun holiday-days-from-easter (n year)
  "Add N days to the date of Easter in YEAR."
  (calendar-gregorian-from-absolute (+ n (calendar-easter-date year))))


;;; Config support

(defun cb-org/display-links ()
  (interactive)
  (let ((bufname "*Org Links*"))
    (-if-let (buf (get-buffer bufname))
        (display-buffer buf)
      (with-current-buffer (find-file-noselect org-default-notes-file)
        (-when-let (mark (save-excursion (org-find-exact-headline-in-buffer "Links")))
          (clone-indirect-buffer bufname t)
          (goto-char (marker-position mark))
          (org-narrow-to-subtree)
          (org-content))))))

;; HACK: override `org-clocktable-steps' to customise clocktable appearance.
(with-eval-after-load 'org-clock

  (defun org-clocktable-steps (params)
    "Step through the range to make a number of clock tables."
    (let* ((p1 (copy-sequence params))
           (ts (plist-get p1 :tstart))
           (te (plist-get p1 :tend))
           (ws (plist-get p1 :wstart))
           (ms (plist-get p1 :mstart))
           (step0 (plist-get p1 :step))
           (step (cdr (assoc step0 '((day . 86400) (week . 604800)))))
           (stepskip0 (plist-get p1 :stepskip0))
           (block (plist-get p1 :block))
           cc range-text step-time tsb)
      (when block
        (setq cc (org-clock-special-range block nil t ws ms)
              ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
      (cond
       ((numberp ts)
        ;; If ts is a number, it's an absolute day number from org-agenda.
        (-let [(month day year) (calendar-gregorian-from-absolute ts)]
          (setq ts (org-float-time (encode-time 0 0 0 day month year)))))
       (ts
        (setq ts (org-float-time
                  (apply 'encode-time (org-parse-time-string ts))))))
      (cond
       ((numberp te)
        ;; Likewise for te.
        (-let [(month day year) (calendar-gregorian-from-absolute te)]
          (setq te (org-float-time (encode-time 0 0 0 day month year)))))
       (te
        (setq te (org-float-time
                  (apply 'encode-time (org-parse-time-string te))))))
      (setq tsb
            (if (eq step0 'week)
                (- ts (* 86400 (- (nth 6 (decode-time (seconds-to-time ts))) ws)))
              ts))
      (setq p1 (plist-put p1 :header ""))
      (setq p1 (plist-put p1 :step nil))
      (setq p1 (plist-put p1 :block nil))
      (while (< tsb te)
        (or (bolp) (insert "\n"))
        (setq p1 (plist-put p1 :tstart (format-time-string
                                        (org-time-stamp-format nil t)
                                        (seconds-to-time (max tsb ts)))))
        (setq p1 (plist-put p1 :tend (format-time-string
                                      (org-time-stamp-format nil t)
                                      (seconds-to-time (min te (setq tsb (+ tsb step)))))))
        (insert "\n\n" (if (eq step0 'day) "Daily report: "
                         "Weekly report starting on: ")
                (plist-get p1 :tstart) "\n")
        (setq step-time (org-dblock-write:clocktable p1))
        (re-search-forward "^[ \t]*#\\+END:")
        (when (and (equal step-time 0) stepskip0)
          ;; Remove the empty table
          (delete-region (point-at-bol)
                         (save-excursion
                           (re-search-backward "^\\(Daily\\|Weekly\\) report"
                                               nil t)
                           (point))))
        (end-of-line 0))))
  )

;;; funcs.el ends here
