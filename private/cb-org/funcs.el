;;; funcs.el --- supporting functions for org config.
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'org nil t)
  (require 'dash nil t)
  (require 's nil t)
  )

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
  "Call `org-table-hline-and-move' or `org-insert-todo-heading' dep. on context."
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

(autoload 'appt-check "appt")

(defun cb-org/diary-update-appt-on-save ()
  (save-restriction
    (save-window-excursion
      (org-agenda-to-appt t)
      (appt-check 'force))))


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
