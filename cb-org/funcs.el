;;; funcs.el --- supporting functions for org config.
;;; Commentary:
;;; Code:

(require 'calendar)
(require 'dash)
(require 'f)
(require 's)

(autoload 'org-at-heading-p "org")
(autoload 'org-back-to-heading "org")
(autoload 'org-clock-special-range "org-clock")
(autoload 'org-dblock-write:clocktable "org-clock")
(autoload 'org-float-time "org-compat")
(autoload 'org-parse-time-string "org")
(autoload 'org-time-stamp-format "org")

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

;; HACK: Fix screen redraw issue that prevents org-drill from displaying
;; content.
;;
;; https://bitbucket.org/eeeickythump/org-drill/issues/28/org-drill-randomly-shows-nothing

(with-eval-after-load 'org
  (defun org-toggle-latex-fragment (&optional arg)
    "Preview the LaTeX fragment at point, or all locally or globally.

If the cursor is on a LaTeX fragment, create the image and overlay
it over the source code, if there is none.  Remove it otherwise.
If there is no fragment at point, display all fragments in the
current section.

With prefix ARG, preview or clear image for all fragments in the
current subtree or in the whole buffer when used before the first
headline.  With a double prefix ARG \\[universal-argument] \
\\[universal-argument] preview or clear images
for all fragments in the buffer."
    (interactive "P")
    (unless (buffer-file-name (buffer-base-buffer))
      (user-error "Can't preview LaTeX fragment in a non-file buffer"))
    (when (display-graphic-p)
      (catch 'exit
        (save-excursion
          (let ((window-start (window-start)) msg)
            (save-restriction
              (cond
               ((or (equal arg '(16))
                    (and (equal arg '(4))
                         (org-with-limited-levels (org-before-first-heading-p))))
                (if (org-remove-latex-fragment-image-overlays)
                    (progn (message "LaTeX fragments images removed from buffer")
                           (throw 'exit nil))
                  (setq msg "Creating images for buffer...")))
               ((equal arg '(4))
                (org-with-limited-levels (org-back-to-heading t))
                (let ((beg (point))
                      (end (progn (org-end-of-subtree t) (point))))
                  (if (org-remove-latex-fragment-image-overlays beg end)
                      (progn
                        (message "LaTeX fragment images removed from subtree")
                        (throw 'exit nil))
                    (setq msg "Creating images for subtree...")
                    (narrow-to-region beg end))))
               ((let ((datum (org-element-context)))
                  (when (memq (org-element-type datum)
                              '(latex-environment latex-fragment))
                    (let* ((beg (org-element-property :begin datum))
                           (end (org-element-property :end datum)))
                      (if (org-remove-latex-fragment-image-overlays beg end)
                          (progn (message "LaTeX fragment image removed")
                                 (throw 'exit nil))
                        (narrow-to-region beg end)
                        (setq msg "Creating image..."))))))
               (t
                (org-with-limited-levels
                 (let ((beg (if (org-at-heading-p) (line-beginning-position)
                              (outline-previous-heading)
                              (point)))
                       (end (progn (outline-next-heading) (point))))
                   (if (org-remove-latex-fragment-image-overlays beg end)
                       (progn
                         (message "LaTeX fragment images removed from section")
                         (throw 'exit nil))
                     (setq msg "Creating images for section...")
                     (narrow-to-region beg end))))))
              (let ((file (buffer-file-name (buffer-base-buffer))))
                (org-format-latex
                 (concat org-latex-preview-ltxpng-directory
                         (file-name-sans-extension (file-name-nondirectory file)))
                 ;; Emacs cannot overlay images from remote hosts.
                 ;; Create it in `temporary-file-directory' instead.
                 (if (file-remote-p file) temporary-file-directory
                   default-directory)
                 'overlays msg 'forbuffer
                 org-latex-create-formula-image-program)))
            ;; Work around a bug that doesn't restore window's start
            ;; when widening back the buffer.

            ;; HACK: commented out. See note above.
            ;; (set-window-start nil window-start)

            (message (concat msg "done"))))))))

;;; funcs.el ends here
