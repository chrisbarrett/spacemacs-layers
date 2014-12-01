(defvar cb-org-pre-extensions
  '(
    ;; pre extension cb-orgs go here
    org-work
    org-agenda
    org-indent
    )
  "List of all extensions to load before the packages.")

(defvar cb-org-post-extensions
  '(
    ;; post extension cb-orgs go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-org/init-<extension-cb-org>
;;
;; (defun cb-org/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-org/init-org-work ()
  (use-package org-work
    :commands org-work-maybe-start-work
    :init
    (add-hook 'after-init-hook 'org-work-maybe-start-work)
    :config
    (add-hook 'org-work-state-changed-hook 'org/refresh-agenda-when-toggling-work)))

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
      (setq org-agenda-auto-exclude-function 'org/exclude-tasks-on-hold)
      (setq org-agenda-diary-file (concat org-directory "diary.org"))
      (setq org-agenda-hide-tags-regexp (rx (or "noexport" "someday")))
      (setq org-agenda-insert-diary-extract-time t)
      (setq org-agenda-ndays 7)
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
      (setq org-agenda-span 'week)
      (setq org-agenda-start-on-weekday nil)
      (setq org-agenda-text-search-extra-files '(agenda-archives))

      (setq org-agenda-custom-commands
         (->> '(
                ("A" "Agenda and next actions"
                 ((tags-todo "-someday-media/NEXT"
                             ((org-agenda-overriding-header "Next Actions")))
                  (agenda "")
                  (todo "WAITING"
                        ((org-agenda-overriding-header "Waiting")))
                  (stuck "")
                  (tags-todo "media/NEXT"
                             ((org-agenda-overriding-header "Media"))))
                 ((org-agenda-tag-filter-preset
                   '("-@work"))))

                ("w" "Agenda and work actions"
                 ((tags-todo "-someday/NEXT"
                             ((org-agenda-overriding-header "Next Actions")))
                  (agenda ""
                          ((org-agenda-span 'fortnight)))
                  (todo "WAITING|ORGANISE_IN|COLLECT"
                        ((org-agenda-overriding-header "Incoming/Waiting")))
                  (todo "TODO_OUT|READY|ORGANISE_OUT"
                        ((org-agenda-overriding-header "Outgoing")))
                  (stuck "")
                  )
                 ((org-agenda-tag-filter-preset
                   '("+@work" "-ignore"))
                  (org-agenda-files (list org-work-file))
                  (org-deadline-warning-days 0)
                  (org-agenda-todo-ignore-deadlines 14)
                  (org-agenda-todo-ignore-scheduled 'all)
                  (org-agenda-hide-tags-regexp
                   (regexp-opt
                    (list org-agenda-hide-tags-regexp "@work")))))

                ("n" "Next actions"
                 ((tags-todo "-someday/NEXT"))
                 ((org-agenda-overriding-header "Next Actions")))

                ("g" . "GTD contexts")
                ("gg" "Anywhere"
                 ((tags-todo "@computer")
                  (tags-todo "@errand")
                  (tags-todo "@home")
                  (tags-todo "@leisure")
                  (tags-todo "@phone")
                  (tags-todo "@work")))
                ("gc" "Computer" tags-todo "@computer")
                ("ge" "Errands" tags-todo "@errand")
                ("gp" "Phone" tags-todo "@phone")
                ("gw" "Work" tags-todo "@work")
                ("gh" "Home" tags-todo "@home")
                ("gl" "Leisure" tags-todo "@leisure")

                ("r" "Weekly Review"
                 ((agenda ""
                          ((org-agenda-ndays 21)
                           (org-agenda-start-day "-7d")))
                  (stuck "")
                  (todo "WAITING"
                        ((org-agenda-overriding-header "Waiting")))
                  (tags-todo "someday-skill/MAYBE|NEXT"
                             ((org-agenda-overriding-header "Someday")))
                  (tags-todo "someday&skill"
                             ((org-agenda-overriding-header "Skills")))
                  (tags-todo "media"
                             ((org-agenda-overriding-header "Media"))))
                 ((org-agenda-tag-filter-preset
                   '("-drill" "-gtd"))
                  (org-habit-show-habits nil)
                  (org-agenda-include-inactive-timestamps t)))
                )
           (--map-when
            (listp
             (cdr it))
            (append it
                    '(((org-agenda-customise-window-hook 'delete-other-windows)))))))

      (add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)
      (add-to-list 'org-agenda-files org-directory)

      (unless noninteractive
        (add-hook 'after-init-hook 'org/agenda-dwim)))))

(defun cb-org/init-org-indent ()
  (use-package org-indent
    :diminish org-indent-mode))
