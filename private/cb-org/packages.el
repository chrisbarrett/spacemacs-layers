(defvar cb-org-packages
  '(
    ;; package cb-orgs go here
    org
    org-drill-table
    org-jira
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-org-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-org/init-<package-cb-org>
;;
;; (defun cb-org/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-org/init-org ()
  (use-package org
    :init
    (setq org-directory (file-name-as-directory org-directory))
    :config
    (progn
      (require 's)

      (add-hook 'org-mode-hook 'auto-revert-mode)
      (add-hook 'org-mode-hook 'abbrev-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)

      (setq org-default-notes-file (concat org-directory "notes.org"))
      (setq org-M-RET-may-split-line nil)
      (setq org-attach-directory (concat org-directory "data"))
      (setq org-blank-before-new-entry nil)
      (setq org-catch-invisible-edits 'smart)
      (setq org-clock-persist-file (concat org-directory ".org-clock-save"))
      (setq org-completion-use-ido t)
      (setq org-cycle-separator-lines 0)
      (setq org-drawers '("COMMENTS" "NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS"))
      (setq org-enforce-todo-dependencies t)
      (setq org-footnote-auto-adjust t)
      (setq org-id-locations-file (concat spacemacs-cache-directory "org-id-locations"))
      (setq org-indirect-buffer-display 'current-window)
      (setq org-insert-heading-respect-content nil)
      (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
      (setq org-link-mailto-program '(compose-mail "%a" "%s"))
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-pretty-entities t)
      (setq org-put-time-stamp-overlays t)
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))
      (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
      (setq org-refile-use-outline-path t)
      (setq org-return-follows-link t)
      (setq org-reverse-note-order nil)
      (setq org-startup-indented t)
      (setq org-startup-with-inline-images t)
      (setq org-stuck-projects '("-ignore+TODO={TODO_OUT\\|PROJECT}/-MAYBE-DONE-CANCELLED" ("NEXT") nil "SCHEDULED:\\|\\<IGNORE\\>"))
      (setq org-support-shift-select t)
      (setq org-todo-keywords '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                                (type "PROJECT(p)" "|")))
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
      (setq org-capture-templates
            `(("t" "Todo" entry
               (file+olp org-default-notes-file "Tasks")
               "* TODO %?"
               :clock-keep t)

              ("d" "Diary" entry
               (file+datetree org-agenda-diary-file)
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

              ("s" "Someday" entry
               (file+olp org-default-notes-file "Someday")
               "* MAYBE %?"
               :clock-keep t)

              ("S" "Shopping" checkitem
               (file+olp org-default-notes-file "Tasks" "Shopping")
               "- [ ] %?"
               :clock-keep t)

              ("z" "Note" entry
               (file+olp org-default-notes-file "Notes")
               "* %i%?"
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

              ;;; Work items

              ("T" "Work Todo" entry
               (file+olp org-work-file "Tasks")
               "* TODO %?"
               :clock-keep t)

              ("L" "Work Link" entry
               (file+olp org-work-file "Links")
               (function cb-org/read-url-for-capture)
               :immediate-finish t
               :clock-keep t)

              ("Z" "Work Note" entry
               (file+olp org-work-file "Notes")
               "* %i%?"
               :clock-keep t))
            )

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


      (--each '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5
                org-level-6
                org-level-7
                org-level-8)
        (custom-set-faces `(,it ((t :font ,core/monospace-font)))))

      (setq org-todo-keyword-faces
            `(("NEXT" . ,solarized-hl-orange)
              ("ORGANISE_IN" . ,solarized-hl-orange)
              ("ORGANISE_OUT" . ,solarized-hl-orange)
              ("TODO_OUT" . ,solarized-hl-orange)
              ("READY" . ,solarized-hl-blue)
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

      (add-hook 'org-mode-hook 'org/add-local-hooks)
      (add-hook 'org-after-todo-state-change-hook 'org/set-next-todo-state)
      (add-hook 'org-after-todo-statistics-hook 'org/children-done-parent-done))))

(defun cb-org/init-org-drill-table ()
  (use-package org-drill-table
    :config
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-drill-table-update)))

(defun cb-org/init-org-jira ()
  (use-package org-jira
    :defer t))
