;;; packages.el --- cb-org Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-org-packages
  '(
    org
    org-drill-table
    org-jira
    gnuplot
    org-gcal
    ;; required by org-babel for exporting syntax highlighting in code blocks
    htmlize
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-org-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-org/init-org ()
  (use-package org
    :init
    (progn
      (defconst cb-org/default-stuck-projects
        '("-ignore-3_years+TODO={TODO_OUT\\|PROJECT}/-MAYBE-DONE-CANCELLED" ("NEXT") nil "SCHEDULED:\\|\\<IGNORE\\>"))

      (defconst org-directory
        (let ((in-dropbox (f-join user-dropbox-directory "org/")))
          (if (f-exists? in-dropbox) in-dropbox "~/org/"))))
    :config
    (progn
      (require 's)

      (add-hook 'org-mode-hook 'auto-revert-mode)
      (add-hook 'org-mode-hook 'abbrev-mode)

      (defun cb-org/maybe-enable-autofill ()
        (unless (and (boundp 'org-jira-mode) org-jira-mode)
          (turn-on-auto-fill)))

      (add-hook 'org-mode-hook 'cb-org/maybe-enable-autofill)

      (setq org-default-notes-file (f-join org-directory "notes.org"))
      (setq org-M-RET-may-split-line nil)
      (setq org-attach-directory (f-join org-directory "data"))
      (setq org-blank-before-new-entry nil)
      (setq org-catch-invisible-edits 'smart)
      (setq org-clock-persist-file (f-join org-directory ".org-clock-save"))
      (setq org-completion-use-ido t)
      (setq org-cycle-separator-lines 0)
      (setq org-drawers '("COMMENTS" "NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS"))
      (setq org-enforce-todo-dependencies t)
      (setq org-footnote-auto-adjust t)
      (setq org-id-locations-file (f-join spacemacs-cache-directory "org-id-locations"))
      (setq org-indirect-buffer-display 'current-window)
      (setq org-insert-heading-respect-content t)
      (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
      (setq org-link-mailto-program '(compose-mail "%a" "%s"))
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)
      (setq org-hide-emphasis-markers t)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-pretty-entities t)
      (setq org-put-time-stamp-overlays t)
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))
      (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
      (setq org-refile-use-outline-path t)
      (setq org-return-follows-link t)
      (setq org-reverse-note-order nil)
      (setq org-confirm-elisp-link-function nil)
      (setq org-startup-indented t)
      (setq org-startup-with-inline-images t)
      (setq org-stuck-projects cb-org/default-stuck-projects)
      (setq org-support-shift-select t)
      (setq org-todo-keywords '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                                (type "PROJECT(p)" "|")
                                (type "SOMEDAY(S)" "|")))
      (setq org-hierarchical-todo-statistics nil)
      (setq org-checkbox-hierarchical-statistics t)
      (setq org-tag-persistent-alist
            '((:startgroup)
              ("@computer" . 99)
              ("@errand" . 101)
              ("@home" . 104)
              ("@leisure" . 108)
              ("@phone" . 112)
              ("@work" . 119)
              (:endgroup)))

      (setq org-global-properties
            `(("Effort_ALL" . ,(concat "1:00 2:00 3:00 4:00 "
                                       "5:00 6:00 7:00 8:00 9:00 "
                                       "0:05 0:10 0:30"))))

      ;; Faces

      (custom-set-faces
       '(org-hide ((t :background unspecified)))
       '(org-meta-line ((t :italic nil :inherit font-lock-comment-face)))
       '(org-document-info-keyword ((t :foreground unspecified :inherit org-meta-line)))

       `(org-block-begin-line
         ((((background light)) :italic t :foreground ,solarized-hl-cyan)
          (((background dark))  :italic t :foreground ,solarized-hl-cyan)))
       `(org-block-end-line
         ((((background light)) :italic t :foreground ,solarized-hl-cyan)
          (((background dark))  :italic t :foreground ,solarized-hl-cyan)))
       '(org-block-background
         ((((background light)) :background "#f8f1dc")
          (((background dark))  :background "#11303b"))))

      (setq org-todo-keyword-faces
            `(("NEXT" . ,solarized-hl-orange)
              ("ORGANISE_IN" . ,solarized-hl-orange)
              ("ORGANISE_OUT" . ,solarized-hl-orange)
              ("TODO_OUT" . ,solarized-hl-orange)
              ("READY" . ,solarized-hl-blue)
              ("ON-HOL" . ,solarized-hl-magenta)
              ("OPEN" . font-lock-comment-face)
              ("WAITING" . ,solarized-hl-magenta)))

      ;; Advice

      (defadvice org-add-log-note (before exit-minibuffer activate)
        "If the minibuffer is active, exit before prompting for a note."
        (when (minibufferp (window-buffer (selected-window)))
          (other-window 1)))

      (defadvice org-insert-heading (after insert-state activate)
        (when (called-interactively-p nil)
          (evil-insert-state)))

      (defadvice org-insert-heading-respect-content (after insert-state activate)
        (when (called-interactively-p nil)
          (evil-insert-state)))

      (defadvice org-insert-todo-heading (after insert-state activate)
        (when (called-interactively-p nil)
          (evil-insert-state)))

      (defadvice org-insert-todo-heading-respect-content (after insert-state activate)
        (when (called-interactively-p nil)
          (evil-insert-state)))

      (defadvice org-toggle-heading (after goto-line-end activate)
        "Prevent point from being moved to the line beginning."
        (when (s-matches? (rx bol (+ "*") (* space) eol) (current-line))
          (goto-char (line-end-position))))


      ;; Hooks

      (defun cb-org/tidy-org-buffer ()
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
                (org-remove-empty-drawer-at (match-beginning 0)))))))

      (defun cb-org/mark-next-parent-tasks-todo ()
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

      (defun cb-org/add-local-hooks ()
        "Set buffer-local hooks for orgmode."
        (add-hook 'after-save-hook 'cb-org/diary-update-appt-on-save nil t)
        (add-hook 'org-after-todo-state-change-hook 'cb-org/mark-next-parent-tasks-todo nil t)
        (add-hook 'org-clock-in-hook 'cb-org/mark-next-parent-tasks-todo nil t)
        (add-hook 'before-save-hook 'cb-org/tidy-org-buffer nil t))


      (add-hook 'org-mode-hook 'cb-org/add-local-hooks)


      (defun cb-org/set-next-todo-state ()
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

      (add-hook 'org-after-todo-state-change-hook 'cb-org/set-next-todo-state)


      (defun cb-org/children-done-parent-done (n-done n-todo)
        "Mark the parent task as done when all children are completed."
        (let (org-log-done org-log-states) ; turn off logging
          (org-todo (if (zerop n-todo) "DONE" "TODO"))))

      (add-hook 'org-after-todo-statistics-hook 'cb-org/children-done-parent-done))))

(defun cb-org/init-org-drill-table ()
  (use-package org-drill-table
    :config
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-drill-table-update)))

(defun cb-org/init-org-jira ()
  (use-package org-jira
    :commands (org-jira-mode)
    :init
    (progn

      (setq org-jira-use-status-as-todo t)
      (setq org-jira-done-states '("CLOSED" "WONT-DO" "RESOLVED"))

      (setq org-jira-default-jql
            "assignee = currentUser() AND resolution = Unresolved
             ORDER BY priority DESC, created ASC")

      (defconst org-jira-working-dir (f-join org-directory "jira"))

      (defun cb-org/jira-files ()
        (f-files org-jira-working-dir (lambda (f) (f-ext? f "org")) t))

      (defun cb-org/maybe-enable-org-jira ()
        (when (ignore-errors
                (f-descendant-of? (buffer-file-name) org-jira-working-dir))
          (org-jira-mode +1)))

      (add-hook 'org-jira-mode-hook 'turn-off-auto-fill)
      (add-hook 'org-mode-hook 'cb-org/maybe-enable-org-jira)
      )
    :config
    (progn

      (defun cb-org/jira-sync ()
        (interactive)
        (call-interactively 'org-jira-get-issues)
        (--each (cb-org/jira-files)
          (with-current-buffer (find-file-noselect it)
            (save-buffer))))

      (defalias 'org-jira-sync 'cb-org/jira-sync)

      ;; Apply JIRA issue number as the default org category

      (defadvice org-jira-get-issues (after apply-category activate)
        (org-map-entries '(cb-org/apply-to-headlines-at-level 1 'cb-org/maybe-apply-id-as-category)
                         nil
                         (cb-org/jira-files)))

      (defun cb-org/apply-to-headlines-at-level (n fn)
        "Apply FN over headings at level N."
        (let ((level (car (org-heading-components))))
          (when (= n level)
            (funcall fn))))

      (defun cb-org/maybe-apply-id-as-category ()
        (unless (org-entry-get nil "CATEGORY")
          (-when-let (tag (org-entry-get (point) "ID"))
            (org-set-property "CATEGORY" tag))))

      ;; Better feedback on issues

      (defadvice org-jira-get-issues (after notify activate)
        (message "Issues updated."))

      (defadvice org-jira-update-issue (after notify activate)
        (message "Issue updated."))

      (defadvice org-jira-update-issue-details (after notify activate)
        (message "Details updated."))

      (defadvice org-jira-update-comments-for-current-issue (after notify activate)
        (message "Comments updated.")))))

(defun cb-org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :config
    (progn
      (setq gnuplot-image-format "png")
      (setq gnuplot-inline-image-mode 'dedicated)
      (add-hook 'gnuplot-mode-hook 'page-break-lines-mode)

      (defadvice org-plot/gnuplot (around display-buffer activate)
        (ignore-errors ad-do-it)
        (-when-let (buf (get-buffer gnuplot-image-buffer-name))
          (display-buffer buf)))
      )))

(defun cb-org/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun cb-org/init-org-gcal ()
  (use-package org-gcal
    :init
    (defconst cb-org/gcal-directory
      (f-join org-directory "gcal"))
    :config
    (progn
      (setq org-gcal-dir (f-slash (f-join spacemacs-cache-directory "org-gcal/")))
      (setq org-gcal-token-file (f-join org-gcal-dir ".org-gcal-token")))))
