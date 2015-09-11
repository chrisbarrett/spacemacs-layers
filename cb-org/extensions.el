;;; extensions.el --- cb-org Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-org-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defconst cb-org-post-extensions
  '(
    org-work
    org-agenda
    org-indent
    org-archive
    org-table
    org-habit
    org-src
    org-clock
    org-crypt
    org-drill
    org-capture
    ox
    ox-texinfo
    cb-org-latex-preview-retina
    )
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t)
  (require 's nil t)
  (require 'dash nil t)
  (require 'noflet nil t))

(defun cb-org/init-org-work ()
  (use-package org-work
    :commands (org-work-maybe-start-work
               maybe-enable-org-work-mode)
    :init
    (progn
      (add-to-list 'load-path (f-join user-layers-directory "cb-org/extensions/org-work"))
      (add-hook 'org-mode-hook 'maybe-enable-org-work-mode)
      (add-hook 'after-init-hook 'org-work-maybe-start-work))
    :config
    (progn

      (defun cb-org/refresh-agenda-when-toggling-work ()
        "Refresh the agenda when toggling between work states."
        (when (derived-mode-p 'org-agenda-mode)
          (cb-org/agenda-dwim)))

      (add-hook 'org-work-state-changed-hook 'cb-org/refresh-agenda-when-toggling-work))))

(defun cb-org/init-org-agenda ()
  (use-package org-agenda
    :init
    (progn
      (defvar org-agenda-customise-window-hook nil
        "Relay hook for `org-agenda-mode-hook'.  Suitable for setting up the window.")

      (add-hook 'org-agenda-mode-hook
                (lambda ()
                  (run-hooks 'org-agenda-customise-window-hook))))
    :config
    (progn

      (defun cb-org/exclude-tasks-on-hold (tag)
        (and (equal tag "hold") (concat "-" tag)))

      (setq org-agenda-include-diary t)
      (setq org-agenda-start-on-weekday nil)
      (setq org-agenda-auto-exclude-function 'cb-org/exclude-tasks-on-hold)
      (setq org-agenda-diary-file (f-join org-directory "diary.org"))
      (setq org-agenda-hide-tags-regexp (rx (or "noexport" "someday")))
      (setq org-agenda-insert-diary-extract-time t)
      (setq org-agenda-span 'week)
      (setq org-agenda-search-view-always-boolean t)
      (setq org-agenda-show-all-dates nil)
      (setq org-agenda-show-inherited-tags nil)
      (setq org-agenda-skip-deadline-if-done t)
      (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-sorting-strategy
            '((agenda habit-down time-up priority-down category-keep)
              (todo priority-down category-keep scheduled-up)
              (tags priority-down category-keep)
              (search category-keep)))
      (setq org-agenda-text-search-extra-files '(agenda-archives))
      (setq org-agenda-use-time-grid nil)

      (setq org-agenda-clockreport-parameter-plist
            (list :link t
                  :maxlevel 5
                  :indent nil
                  :narrow 40
                  :tcolumns 1
                  :stepskip0 t
                  :fileskip0 t
                  :step 'week))

      (setq org-time-clocksum-format
            (list :hours "%d" :require-hours t
                  :minutes ":%02d" :require-minutes t))

      (add-hook 'after-init-hook 'cb-org/agenda-dwim)

      (defun cb-org/org-file? (path)
        (or (f-ext? path "org")
            (f-ext? path "org_archive")))

      (defun cb-org/all-org-files ()
        (-uniq (-union (cons org-default-notes-file (cb-org/toplevel-files))
                       (cb-org/work-files))))

      (defun cb-org/jira-files ()
        (when (boundp 'org-jira-working-dir)
          (f-files org-jira-working-dir 'cb-org/org-file?)))

      (defun cb-org/gcal-files ()
        (when (boundp 'cb-org/gcal-directory)
          (f-files cb-org/gcal-directory 'cb-org/org-file?)))

      (defun cb-org/toplevel-files ()
        (f-files org-directory (lambda (f)
                                 (and (s-matches? (rx (or "notes" "work" "diary")) (f-filename f))
                                      (cb-org/org-file? f)))))

      (defun cb-org/work-files ()
        (-distinct (-concat (cb-org/jira-files) (cb-org/gcal-files) (cb-org/toplevel-files))))

      (setq org-agenda-files (cb-org/all-org-files))

      (defun cb-org/agenda-custom-commands-delete-other-windows (command-list)
        (-map-when (lambda (spec) (listp (cdr spec)))
                   (lambda (spec) (append spec '(((org-agenda-customise-window-hook 'delete-other-windows)))))
                   command-list))

      (setq org-agenda-custom-commands
            (cb-org/agenda-custom-commands-delete-other-windows
             '(
               ("A" "Agenda and next actions"
                ((tags-todo "-study-@work-someday-media/NEXT"
                            ((org-agenda-overriding-header "Next Actions")))
                 (agenda "-@work")
                 (tags-todo "-@work/WAITING"
                            ((org-agenda-overriding-header "Waiting")))
                 (stuck "-@work")
                 (tags-todo "media|study/NEXT"
                            ((org-agenda-overriding-header "Media & Study"))))
                ((org-agenda-tag-filter-preset
                  '("-work_habit" "-ignore"))
                 (org-agenda-files (cb-org/toplevel-files))))

               ("w" "Agenda and work actions"
                ((tags-todo "-study+assignee=\"\"+TODO={NEXT}|assignee=\"chrisb\"+TODO={NEXT}"
                            ((org-agenda-overriding-header "Next Actions")))
                 (tags-todo "-hold+assignee=\"chrisb\"+TODO={IN-PROGRESS}"
                            ((org-agenda-overriding-header "In Progress")))
                 (todo "WAITING"
                       ((org-agenda-overriding-header "Waiting")))
                 (stuck ""
                        ((org-agenda-overriding-header "Stuck Cards")))
                 (tags-todo "-hold+assignee=\"chrisb\"+TODO={READY-TO-START\\|TO-DO\\|PROJECT}" ; MKG uses TO-DO for cards not started.
                            ((org-agenda-overriding-header "Upcoming Cards")))
                 (tags-todo "+hold+assignee=\"chrisb\"+LEVEL=1|assignee=\"chrisb\"+TODO={ON-HOL}"
                            ((org-agenda-overriding-header "On Hold")))
                 (agenda ""))

                ((org-agenda-tag-filter-preset '("-ignore"))
                 (org-agenda-span 'fortnight)
                 (org-agenda-dim-blocked-tasks nil)
                 (org-agenda-clockreport-mode t)
                 (org-agenda-show-log t)
                 (org-agenda-files (cb-org/work-files))
                 (org-deadline-warning-days 0)
                 (org-agenda-todo-ignore-deadlines 14)
                 (org-agenda-todo-ignore-scheduled 'all)
                 (org-agenda-remove-tags t)
                 (org-use-property-inheritance t)
                 (org-stuck-projects
                  ;; MKG uses TO-DO for cards not started.
                  '("-hold-ignore+TODO={PROJECT\\|IN-PROGRESS\\|TO-DO}+assignee=\"chrisb\"/-RESOLVED-DONE" ("NEXT") nil "SCHEDULED:\\|\\<IGNORE\\>"))
                 ))

               ("n" "Next actions"
                ((tags-todo "-study-someday/NEXT"))
                ((org-agenda-overriding-header "Next Actions")))

               ("r" "Weekly Review"
                ((agenda ""
                         ((org-agenda-overriding-header "Review Previous Week")
                          (org-agenda-ndays 7)
                          (org-agenda-start-day "-7d")
                          (org-agenda-show-log t)))
                 (agenda ""
                         ((org-agenda-overriding-header "Review Upcoming Events")
                          (org-agenda-ndays 14)))
                 (stuck ""
                        ((org-agenda-overriding-header "Review Stuck Projects")))
                 (todo "WAITING"
                       ((org-agenda-overriding-header "Review Tasks on Hold")))

                 (tags-todo "-@work-someday-media/NEXT"
                            ((org-agenda-overriding-header "Next Actions")))
                 (tags-todo "-@work+goals+3_months/PROJECT|NEXT"
                            ((org-agenda-overriding-header "Review 3 Month Goals")))
                 (tags-todo "-@work+goals+1_year/PROJECT|NEXT"
                            ((org-agenda-overriding-header "Review 1 Year Goals")))
                 (tags-todo "-@work+goals+3_years/MAYBE|SOMEDAY|PROJECT|NEXT"
                            ((org-agenda-overriding-header "Review 3 Year Goals")))
                 (tags-todo "someday-skill/MAYBE|NEXT"
                            ((org-agenda-overriding-header "Decide whether to promote any SOMEDAY items to NEXT actions")))
                 (tags-todo "someday&skill"
                            ((org-agenda-overriding-header "Decide whether to promote any learning tasks to NEXT actions"))))
                ((org-agenda-tag-filter-preset
                  '("-drill" "-gtd" "-work_habit" "-habit" "-ignore"))
                 (org-habit-show-habits nil)
                 (org-agenda-include-inactive-timestamps t)
                 (org-agenda-dim-blocked-tasks nil)
                 (org-agenda-files (cb-org/all-org-files)))))))

      (add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)
      (add-hook 'org-mode-hook 'visual-line-mode)
      (add-hook 'org-mode-hook 'turn-off-auto-fill)
      )))

(defun cb-org/init-org-indent ()
  (use-package org-indent
    :diminish org-indent-mode))

(defun cb-org/init-org-archive ()
  (use-package org-archive
    :config
    (progn

      (defun cb-org/archive-done-tasks ()
        (interactive)
        (atomic-change-group
          (org-map-entries (lambda ()
                             ;; HACK: Ensure point does not move past the next
                             ;; item to archive.
                             (let ((org-map-continue-from (point)))
                               (org-archive-subtree)))
                           "/DONE|PAID|VOID|CANCELLED" 'tree)))

      (setq org-archive-default-command 'cb-org/archive-done-tasks)

      (defadvice org-archive-subtree (before apply-inherited-tags activate)
        (org-set-tags-to (org-get-tags-at))))))

(defun cb-org/init-org-table ()
  (use-package org-table
    :config
    (progn

      (defun cb-org/recalculate-whole-table ()
        "Recalculate the current table using `org-table-recalculate'."
        (interactive "*")
        (when (org-at-table-p)
          (let ((before (buffer-substring (org-table-begin) (org-table-end))))
            (org-table-recalculate '(16))
            (let ((after (buffer-substring (org-table-begin) (org-table-end))))
              (if (equal before after)
                  (message "Table up-to-date")
                (message "Table updated"))))))

      (add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org/recalculate-whole-table))))

(defun cb-org/init-org-habit ()
  (use-package org-habit
    :config
    (progn
      (setq org-habit-preceding-days 14)
      (setq org-habit-following-days 4)
      (setq org-habit-graph-column 70))))

(defun cb-org/init-org-src ()
  (use-package org-src
    :defer t
    :config
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)
         (C . t)
         (ditaa . t)
         (sh . t)
         (calc . t)
         (scala . t)
         (sqlite . t)
         (emacs-lisp . t)
         (gnuplot . t)
         (ruby . t)
         (clojure . t)
         (haskell . t)))

      (setq org-src-fontify-natively t)

      (defvar org-edit-src-before-exit-hook nil
        "Hook run before exiting a code block.")

      (defadvice org-edit-src-exit (before run-hook activate)
        "Run a hook when exiting src block."
        (run-hooks 'org-edit-src-before-exit-hook))

      (add-hook 'org-edit-src-before-exit-hook 'delete-trailing-whitespace)
      (add-hook 'org-src-mode-hook
                (lambda () (setq-local require-final-newline nil))))))

(defun cb-org/init-org-clock ()
  (use-package org-clock
    :init nil
    :config
    (progn

      (defun cb-org/project? ()
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

      (defun cb-org/task? ()
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

      (defun cb-org/clock-in-to-next-state (_kw)
        "Move a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO."
        (unless (and (boundp 'org-capture-mode) org-capture-mode)
          (cond
           ((and (-contains? '("TODO") (org-get-todo-state))
                 (cb-org/task?))
            "NEXT")
           ((and (-contains? '("NEXT") (org-get-todo-state))
                 (cb-org/project?))
            "TODO"))))

      (setq org-clock-in-switch-to-state 'cb-org/clock-in-to-next-state)
      (setq org-clock-persist t)
      (setq org-clock-persist-query-resume nil)
      (setq org-clock-history-length 20)
      (setq org-clock-in-resume t)
      (setq org-clock-report-include-clocking-task t)
      (setq org-clock-out-remove-zero-time-clocks t)

      (org-clock-persistence-insinuate)

      (defun cb-org/remove-empty-clock-drawers ()
        "Remove empty clock drawers at point."
        (save-excursion
          (beginning-of-line 0)
          (org-remove-empty-drawer-at (point))))

      (add-hook 'org-clock-out-hook 'cb-org/remove-empty-clock-drawers t))))

(defun cb-org/init-org-crypt ()
  (use-package org-crypt
    :config
    (progn

      (defun cb-org/looking-at-pgp-section?? ()
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

      (defun cb-org/decrypt-entry ()
        (when (cb-org/looking-at-pgp-section??)
          (org-decrypt-entry)
          t))

      (add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org/decrypt-entry)

      (setq org-crypt-disable-auto-save 'encypt)
      (org-crypt-use-before-save-magic)
      (add-to-list 'org-tags-exclude-from-inheritance "crypt"))))

(defun cb-org/init-org-drill ()
  (use-package org-drill
    :commands (org-drill
               org-drill-strip-all-data
               org-drill-cram
               org-drill-tree
               org-drill-resume
               org-drill-merge-buffers
               org-drill-entry
               org-drill-directory
               org-drill-again)
    :config
    (progn
      (setq org-drill-save-buffers-after-drill-sessions-p nil)
      (defadvice org-drill (after save-buffers activate)
        (org-save-all-org-buffers)))))

(defun cb-org/init-ox ()
  (use-package ox
    :defer t
    :init
    (setq org-export-backends '(ascii html latex md koma-letter))
    :config
    (progn
      (setq org-export-exclude-tags '("noexport" "crypt"))
      (setq org-html-html5-fancy t)
      (setq org-html-postamble nil)
      (setq org-html-table-row-tags
            (cons
             '(cond
               (top-row-p "<tr class=\"tr-top\">")
               (bottom-row-p "<tr class=\"tr-bottom\">")
               (t
                (if
                    (=
                     (mod row-number 2)
                     1)
                    "<tr class=\"tr-odd\">" "<tr class=\"tr-even\">")))
             "</tr>"))
      (setq org-html-head-extra
            "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
"))))

(defun cb-org/init-ox-texinfo ()
  (use-package ox-texinfo
    :config
    (progn

      (defun cb-org-export/koma-letter-at-subtree (dest)
        "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name. The PDF will be created at DEST."
        (interactive
         (list (if current-prefix-arg
                   (ido-read-file-name "Destination: " nil nil nil ".pdf")
                 (concat (f-no-ext (buffer-file-name)) ".pdf"))))

        (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
          (cb-org-write-subtree-content tmpfile)
          (with-current-buffer (find-file-noselect tmpfile)
            (unwind-protect
                (-if-let (exported (org-koma-letter-export-to-pdf))
                    (f-move exported dest)
                  (error "Export failed"))
              (kill-buffer)))
          (async-shell-command (format "open %s" (shell-quote-argument dest)))
          (message "opening %s..." dest)))

      (defun cb-org/C-c-C-c-export-koma-letter ()
        "Export the koma letter at point."
        (when (ignore-errors
                (s-matches? (rx "latex_class:" (* space) "koma")
                            (cb-org-subtree-content)))
          (call-interactively 'org-export-koma-letter-at-subtree)
          'export-koma-letter))

      (add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org/C-c-C-c-export-koma-letter t)

      (add-to-list 'org-latex-classes '("koma-letter" "
\\documentclass[paper=A4,pagesize,fromalign=right,
               fromrule=aftername,fromphone,fromemail,
               version=last]{scrlttr2}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{booktabs}
\\usepackage{graphicx}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]")))))

(defun cb-org/init-cb-org-latex-preview-retina ()
  (use-package cb-org-latex-preview-retina))

(defun cb-org/init-org-capture ()
  (use-package org-capture
    :config
    (progn

      (defun cb-org/parse-html-title (html)
        "Extract the title from an HTML document."
        (-let (((_ title) (s-match (rx "<title>" (group (* nonl)) "</title>") html))
               ((_ charset) (-map 'intern (s-match (rx "charset=" (group (+ (any "-" alnum)))) html))))
          (if (-contains? coding-system-list charset)
              (decode-coding-string title charset)
            title)))

      (defun cb-org/url-retrieve-html (url)
        "Download the resource at URL and attempt to extract an HTML title."
        (unless (s-matches? (rx "." (or "pdf" "mov" "mp4" "m4v" "aiff" "wav" "mp3") eol) url)
          (with-current-buffer (url-retrieve-synchronously url t)
            (buffer-string))))

      (defun cb-org/last-url-kill ()
        "Return the most recent URL in the kill ring or X pasteboard."
        (--first (s-matches? (rx bos (or "http" "https" "www")) it)
                 (cons (current-kill 0 t) kill-ring)))

      (defun cb-org/read-url-for-capture ()
        "Return a capture template string for a URL org-capture."
        (let* ((url (core/read-string-with-default "URL" (or
                                                          (thing-at-point-url-at-point)
                                                          (cb-org/last-url-kill))))
               (title (cb-org/parse-html-title (cb-org/url-retrieve-html url))))
          (format "* [[%s][%s]]" url (or title url))))

      (setq org-capture-templates
            `(
              ("t" "Todo" entry
               (file+olp org-default-notes-file "Tasks")
               "* TODO %?"
               :clock-keep t)

              ("T" "Todo (work)" entry
               (file+olp org-work-file "Tasks")
               "* TODO %?"
               :clock-keep t)


              ("n" "Next Action" entry
               (file+olp org-default-notes-file "Tasks")
               "* NEXT %?"
               :clock-keep t)

              ("N" "Next Action (work)" entry
               (file+olp org-work-file "Tasks")
               "* NEXT %?"
               :clock-keep t)


              ("d" "Diary" entry
               (file+datetree org-agenda-diary-file)
               "* %?\n%^t"
               :clock-keep t)

              ("D" "Diary (work)" entry
               (file+datetree org-work-file)
               "* %?\n%^t"
               :clock-keep t)

              ("h" "Habit" entry
               (file+olp org-default-notes-file "Habits/Recurring")
               ,(s-unlines
                 "* TODO %?"
                 "SCHEDULED: %t"
                 ":PROPERTIES:"
                 ":STYLE: habit"
                 ":END:")
               :clock-keep t)


              ("l" "Link" entry
               (file+olp org-default-notes-file "Links")
               (function cb-org/read-url-for-capture)
               :immediate-finish t
               :clock-keep t)

              ("L" "Link (work)" entry
               (file+olp org-work-file "Links")
               (function cb-org/read-url-for-capture)
               :immediate-finish t
               :clock-keep t)



              ("s" "Someday" entry
               (file+olp org-default-notes-file "Someday")
               "* SOMEDAY %?"
               :clock-keep t)

              ("m" "Listening" entry
               (file+olp org-default-notes-file "Media" "Listening")
               "* MAYBE Listen to %i%?"
               :clock-keep t)

              ("v" "Viewing" entry
               (file+olp org-default-notes-file "Media" "Viewing")
               "* MAYBE Watch %i%?"
               :clock-keep t)

              ("r" "Reading" entry
               (file+olp org-default-notes-file "Media" "Reading")
               "* MAYBE Read %i%?"
               :clock-keep t)

              )))))
