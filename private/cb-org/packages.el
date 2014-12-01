(defvar cb-org-packages
  '(
    ;; package cb-orgs go here
    org
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
    :config
    (progn

      (add-hook 'org-mode-hook 'auto-revert-mode)
      (add-hook 'org-mode-hook 'abbrev-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)

      (custom-set-variables
       '(org-default-notes-file (concat org-directory "notes.org"))
       '(org-M-RET-may-split-line nil)
       '(org-attach-directory (concat org-directory "data"))
       '(org-blank-before-new-entry nil)
       '(org-catch-invisible-edits 'smart)
       '(org-clock-persist-file (concat org-directory ".org-clock-save"))
       '(org-completion-use-ido t)
       '(org-cycle-separator-lines 0)
       '(org-drawers '("COMMENTS" "NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS"))
       '(org-enforce-todo-dependencies t)
       '(org-footnote-auto-adjust t)
       '(org-id-locations-file (concat cb:tmp-dir "org-id-locations"))
       '(org-indirect-buffer-display 'current-window)
       '(org-insert-heading-respect-content nil)
       '(org-link-abbrev-alist '(("att" . org-attach-expand-link)))
       '(org-link-mailto-program '(compose-mail "%a" "%s"))
       '(org-log-done 'time)
       '(org-log-into-drawer t)
       '(org-outline-path-complete-in-steps nil)
       '(org-pretty-entities t)
       '(org-put-time-stamp-overlays t)
       '(org-refile-allow-creating-parent-nodes 'confirm)
       '(org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))
       '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
       '(org-refile-use-outline-path t)
       '(org-return-follows-link t)
       '(org-reverse-note-order nil)
       '(org-startup-indented t)
       '(org-startup-with-inline-images t)
       '(org-stuck-projects '("-ignore+TODO={TODO_OUT\\|PROJECT}/-MAYBE-DONE-CANCELLED" ("NEXT") nil "\\<IGNORE\\>"))
       '(org-support-shift-select t)
       '(org-todo-keywords '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                             (type "PROJECT(p)")))
       '(org-hierarchical-todo-statistics nil)
       '(org-checkbox-hierarchical-statistics t)
       '(org-tag-persistent-alist
         '((:startgroup)
           ("@computer" . 99)
           ("@errand" . 101)
           ("@home" . 104)
           ("@leisure" . 108)
           ("@phone" . 112)
           ("@work" . 119)
           (:endgroup)))
       '(org-capture-templates
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
            "* %c\n%i"
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
            :clock-keep t)))

       '(org-global-properties
         `(("Effort_ALL" . ,(concat "1:00 2:00 3:00 4:00 "
                                    "5:00 6:00 7:00 8:00 9:00 "
                                    "0:05 0:10 0:30")))))

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

      (add-hook 'org-mode-hook 'org/add-local-hooks)
      (add-hook 'org-after-todo-state-change-hook 'org/set-next-todo-state)
      (add-hook 'org-after-todo-statistics-hook 'org/children-done-parent-done))))
