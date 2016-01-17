;;; packages.el --- cb-org Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(require 's)
(require 'f)

(defconst org-directory
  (let ((in-dropbox (f-join user-dropbox-directory "org/")))
    (if (f-exists? in-dropbox) in-dropbox "~/org/")))

(defconst cb-org-packages
  '(org
    org-drill-table
    gnuplot
    org-agenda
    org-indent
    org-archive
    org-table
    org-src
    org-clock
    org-crypt
    org-drill
    org-capture
    ox
    ox-texinfo
    org-present
    (org-work :location local)
    (cb-org-latex-preview-retina :location local)))

;; HACK: Set aliases for incorrectly-prefixed outline functions required by org.
(defalias 'outline-show-children 'show-children)
(defalias 'outline-hide-subtree 'hide-subtree)
(defalias 'outline-show-subtree 'show-subtree)
(defalias 'outline-show-branches 'show-branches)
(defalias 'outline-hide-sublevels 'hide-sublevels)
(defalias 'outline-hide-other 'hide-other)
(defalias 'outline-hide-leaves 'hide-leaves)
(defalias 'outline-hide-body 'hide-body)
(defalias 'outline-hide-region-body 'hide-region-body)
(defalias 'outline-hide-entry 'hide-entry)
(defalias 'outline-show-entry 'show-entry)
(defalias 'outline-show-all 'show-all)

(defun cb-org/post-init-org-present ()
  (setq org-present-text-scale 4)

  ;; Disable flyspell during presentations.
  (defvar-local cb-org/use-flyspell? nil)
  (defun cb-org/set-use-flyspell () (setq cb-org/use-flyspell? t))
  (defun cb-org/maybe-reenable-flyspell () (when cb-org/use-flyspell? (flyspell-mode +1)))

  (add-hook 'flyspell-mode-hook 'cb-org/set-use-flyspell)
  (add-hook 'org-present-mode-hook 'turn-off-flyspell)
  (add-hook 'org-present-mode-quit-hook 'cb-org/maybe-reenable-flyspell)

  (add-hook 'org-present-mode-hook 'spacemacs/toggle-mode-line-on)
  (add-hook 'org-present-mode-quit-hook 'spacemacs/toggle-mode-line-off))

(defun cb-org/post-init-org ()
  (defconst cb-org/default-stuck-projects
    '("-ignore-3_years+TODO={TODO_OUT\\|PROJECT}/-MAYBE-DONE-CANCELLED" ("NEXT") nil "SCHEDULED:\\|\\<IGNORE\\>"))

  (add-hook 'org-mode-hook 'auto-revert-mode)
  (add-hook 'org-mode-hook 'abbrev-mode)

  (setq org-default-notes-file (f-join org-directory "notes.org"))
  (setq org-M-RET-may-split-line nil)
  (setq org-attach-directory (f-join org-directory "data"))
  (setq org-catch-invisible-edits 'smart)
  (setq org-clock-persist-file (f-join org-directory ".org-clock-save"))
  (setq org-completion-use-ido t)
  (setq org-cycle-separator-lines 1)
  (setq org-enforce-todo-dependencies t)
  (setq org-footnote-auto-adjust t)
  (setq org-id-locations-file (f-join spacemacs-cache-directory "org-id-locations"))
  (setq org-indirect-buffer-display 'current-window)
  (setq org-insert-heading-respect-content t)
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-pretty-entities t)
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
  (setq org-hierarchical-todo-statistics nil)
  (setq org-checkbox-hierarchical-statistics t)
  (setq org-log-repeat nil)

  (setq org-todo-keywords '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                            (type "PROJECT(p)" "|")
                            (type "SOMEDAY(S)" "|")))

  ;; Faces


  (custom-set-faces
   '(org-hide ((t :background unspecified)))
   '(org-meta-line ((t :italic nil :inherit font-lock-comment-face)))
   '(org-document-info-keyword ((t :foreground unspecified :inherit org-meta-line)))

   `(org-block-begin-line
     ((((background light)) :italic t :foreground ,solarized-hl-cyan :background nil)
      (((background dark))  :italic t :foreground ,solarized-hl-cyan :background nil)))
   `(org-block-end-line
     ((((background light)) :italic t :foreground ,solarized-hl-cyan :background nil)
      (((background dark))  :italic t :foreground ,solarized-hl-cyan :background nil)))
   '(org-block
     ((((background light)) :background nil)
      (((background dark))  :background nil)))
   '(org-block-background
     ((((background light)) :background nil)
      (((background dark))  :background nil))))

  (setq org-todo-keyword-faces
        `(("NEXT" . ,solarized-hl-orange)
          ("ORGANISE_IN" . ,solarized-hl-orange)
          ("ORGANISE_OUT" . ,solarized-hl-orange)
          ("TODO_OUT" . ,solarized-hl-orange)
          ("READY" . ,solarized-hl-blue)
          ("ON-HOL" . ,solarized-hl-magenta)
          ("OPEN" . font-lock-comment-face)
          ("WAITING" . ,solarized-hl-magenta)))

  ;; Override themes which set weird headline properties.

  (let ((class '((class color) (min-colors 89))))
    (custom-set-faces
     `(org-level-1 ((,class (:background nil :overline nil :height 1.0))))
     `(org-level-2 ((,class (:background nil :height 1.0))))
     `(org-level-3 ((,class (:background nil :height 1.0))))
     `(org-level-4 ((,class (:background nil :height 1.0))))
     `(org-level-5 ((,class (:background nil :height 1.0))))
     `(org-level-6 ((,class (:background nil :height 1.0))))
     `(org-level-7 ((,class (:background nil :height 1.0))))
     `(org-level-8 ((,class (:background nil :height 1.0))))


     `(org-agenda-done ((,class (:background nil :height 1.0))))
     `(org-scheduled-today ((,class (:background nil :height 1.0))))
     `(org-scheduled-previously ((,class (:background nil :height 1.0))))
     ))

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
    (when (s-matches? (rx bol (+ "*") (* space) eol) (cb-buffers-current-line))
      (goto-char (line-end-position))))


  ;; Hooks

  (defun cb-org/tidy-org-buffer ()
    "Perform cosmetic fixes to the current org buffer."
    (interactive)
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
    (add-hook 'org-after-todo-state-change-hook 'cb-org/mark-next-parent-tasks-todo nil t)
    (add-hook 'org-clock-in-hook 'cb-org/mark-next-parent-tasks-todo nil t))


  (add-hook 'org-mode-hook 'cb-org/add-local-hooks)


  (defun cb-org/set-next-todo-state ()
    "When marking a todo to DONE, set the next TODO as NEXT.
Do not scheduled items or repeating todos."
    (when (equal org-state "DONE")
      (save-excursion
        (when (and (ignore-errors (outline-forward-same-level 1) t)
                   (equal (org-get-todo-state) "TODO"))
          (unless (or (org-entry-get (point) "STYLE")
                      (org-entry-get (point) "LAST_REPEAT")
                      (org-get-scheduled-time (point)))
            (org-todo "NEXT"))))))

  (add-hook 'org-after-todo-state-change-hook 'cb-org/set-next-todo-state)


  (defun cb-org/children-done-parent-done (n-done n-todo)
    "Mark the parent task as done when all children are completed."
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (zerop n-todo) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'cb-org/children-done-parent-done)

  ;; LaTeX preview on C-c C-c

  (defun cb-org/latex-preview-fragment-at-pt ()
    (when (memq (org-element-type (org-element-context))
                '(latex-environment latex-fragment))
      (org-toggle-latex-fragment)
      t))

  (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org/latex-preview-fragment-at-pt t)

  ;;; Keybindings

  (spacemacs/declare-prefix "o" "org")
  (spacemacs/set-leader-keys "oa" #'cb-org/agenda-dwim)
  (spacemacs/set-leader-keys "ob" #'org-iswitchb)
  (spacemacs/set-leader-keys "oc" #'org-clock-goto)
  (spacemacs/set-leader-keys "od" #'cb-org/goto-diary)
  (spacemacs/set-leader-keys "ok" #'org-capture)
  (spacemacs/set-leader-keys "os" #'org-search-view)
  (spacemacs/set-leader-keys "on" #'cb-org/goto-notes)
  (spacemacs/set-leader-keys "ow" #'cb-org/goto-work)
  (spacemacs/set-leader-keys "ot" #'cb-org/todo-list)
  (spacemacs/set-leader-keys "ov" #'cb-org/tags-list)

  ;; HACK: Override clashing keybinding
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-c $") nil))

  (spacemacs/set-leader-keys "oh" #'helm-org-agenda-files-headings)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "mP" #'org-plot/gnuplot)

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-.") #'org-time-stamp-inactive)
    (define-key org-mode-map (kbd "M-p")     #'org-metaup)
    (define-key org-mode-map (kbd "M-n")     #'org-metadown)
    (define-key org-mode-map (kbd "C-c c")   #'org-columns)
    (define-key org-mode-map (kbd "C-c C-k") #'cb-org/ctrl-c-ctrl-k)
    (define-key org-mode-map (kbd "C-c RET") #'cb-org/ctrl-c-ret)
    (define-key org-mode-map (kbd "C-c ;")   nil)
    (define-key org-mode-map (kbd "M-C-g")   #'org-plot/gnuplot)

    (evil-define-key 'normal org-mode-map (kbd "RET") #'org-return))

  ;; Remove ahs keys that override org keybindings
  (with-eval-after-load 'auto-highlight-symbol
    (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)))

(defun cb-org/init-org-drill-table ()
  (use-package org-drill-table
    :config
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-drill-table-update)))

(defun cb-org/post-init-gnuplot ()
  (setq gnuplot-image-format "png")
  (setq gnuplot-inline-image-mode 'dedicated)
  (add-hook 'gnuplot-mode-hook 'page-break-lines-mode)

  (defadvice org-plot/gnuplot (around display-buffer activate)
    (ignore-errors ad-do-it)
    (-when-let (buf (get-buffer gnuplot-image-buffer-name))
      (display-buffer buf))))

(defun cb-org/init-org-work ()
  (require 'org-work)

  (defun cb-org/refresh-agenda-when-toggling-work ()
    "Refresh the agenda when toggling between work states."
    (when (derived-mode-p 'org-agenda-mode)
      (cb-org/agenda-dwim)))

  (add-hook 'org-work-state-changed-hook 'cb-org/refresh-agenda-when-toggling-work)
  (add-hook 'org-mode-hook 'maybe-enable-org-work-mode)
  (add-hook 'after-init-hook 'org-work-maybe-start-work))

(defun cb-org/init-cb-org-latex-preview-retina ()
  (use-package cb-org-latex-preview-retina))

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
    (define-key org-agenda-mode-map (kbd "C-f") 'evil-scroll-page-down)
    (define-key org-agenda-mode-map (kbd "C-b") 'evil-scroll-page-up)

    (defun cb-org/exclude-tasks-on-hold (tag)
      (and (equal tag "hold") (concat "-" tag)))

    (setq org-agenda-include-diary t)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-auto-exclude-function 'cb-org/exclude-tasks-on-hold)
    (setq org-agenda-files (f-files org-directory (lambda (f) (f-ext? f "org"))))
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
          '((agenda time-up priority-down category-keep)
            (todo priority-down category-keep scheduled-up)
            (tags priority-down category-keep)
            (search category-keep)))
    (setq org-agenda-text-search-extra-files '(agenda-archives))
    (setq org-agenda-use-time-grid nil)

    (setq org-agenda-clockreport-parameter-plist
          (list
           :compact t
           :maxlevel 5
           :fileskip0 t
           :step 'week))

    (setq org-time-clocksum-format
          (list :hours "%d" :require-hours t
                :minutes ":%02d" :require-minutes t))

    (add-hook 'after-init-hook 'cb-org/agenda-dwim)

    (defun cb-org/agenda-custom-commands-delete-other-windows (command-list)
      (-map-when (lambda (spec) (listp (cdr spec)))
                 (lambda (spec) (append spec '(((org-agenda-customise-window-hook 'delete-other-windows)))))
                 command-list))

    (setq org-agenda-custom-commands
          (cb-org/agenda-custom-commands-delete-other-windows
           '(
             ("A" "Agenda and next actions"
              ((tags-todo "-study-someday-media/NEXT"
                          ((org-agenda-overriding-header "Next Actions")))
               (agenda "")
               (tags-todo "WAITING"
                          ((org-agenda-overriding-header "Waiting")))
               (stuck "")
               (tags-todo "media|study/NEXT"
                          ((org-agenda-overriding-header "Media & Study"))))
              ((org-agenda-tag-filter-preset '( "-ignore"))))

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
                '("-drill" "-gtd" "-ignore"))
               (org-agenda-include-inactive-timestamps t)
               (org-agenda-dim-blocked-tasks nil))))))

    (define-key org-agenda-mode-map (kbd "J") 'org-agenda-goto-date)

    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'turn-off-auto-fill)
    ))
    (setq appt-message-warning-time 60)

    (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

(use-package org-indent
  :diminish org-indent-mode)

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
      (org-set-tags-to (org-get-tags-at)))))

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

    (add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org/recalculate-whole-table)))

(use-package org-src
  :defer t
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       ;; (C . t)
       ;; (ditaa . t)
       (sh . t)
       ;; (calc . t)
       (scala . t)
       ;; (sqlite . t)
       (emacs-lisp . t)
       (gnuplot . t)
       ;; (ruby . t)
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
              (lambda () (setq-local require-final-newline nil)))))

(use-package org-clock
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

    (add-hook 'org-clock-out-hook 'cb-org/remove-empty-clock-drawers t)))

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
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")))

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
      (org-save-all-org-buffers))))

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
")))

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
[PACKAGES]"))))

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

            ))))

;;; packages.el ends here
