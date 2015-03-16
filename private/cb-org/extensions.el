(defvar cb-org-pre-extensions
  '(
    ;; pre extension cb-orgs go here
    )
  "List of all extensions to load before the packages.")

(defvar cb-org-post-extensions
  '(
    ;; post extension cb-orgs go here
    org-work
    org-agenda
    org-indent
    org-archive
    org-table
    org-protocol
    org-habit
    org-src
    org-clock
    org-crypt
    org-drill
    org-export
    ox-texinfo
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

(require 's)
(require 'dash)

(defun cb-org/init-org-work ()
  (use-package org-work
    :load-path "private/cb-org/extensions/org-work"
    :commands (org-work-maybe-start-work
               maybe-enable-org-work-mode)
    :init
    (progn
      (add-hook 'org-mode-hook 'maybe-enable-org-work-mode)
      (add-hook 'after-init-hook 'org-work-maybe-start-work))
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
      (setq org-agenda-diary-file (f-join org-directory "diary.org"))
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
      (setq org-agenda-sticky t)

      (defun cb-org/agenda-custom-commands-delete-other-windows (command-list)
        (-map-when (lambda (spec) (listp (cdr spec)))
                   (lambda (spec) (append spec '(((org-agenda-customise-window-hook 'delete-other-windows)))))
                   command-list))

      (setq org-agenda-custom-commands
            (cb-org/agenda-custom-commands-delete-other-windows
             '(
               ("A" "Agenda and next actions"
                ((tags-todo "-@work-someday-media/NEXT"
                            ((org-agenda-overriding-header "Next Actions")))
                 (agenda "-@work")
                 (todo "-@work/WAITING"
                       ((org-agenda-overriding-header "Waiting")))
                 (stuck "-@work")
                 (tags-todo "media|study/NEXT"
                            ((org-agenda-overriding-header "Media & Study"))))
                ((org-agenda-tag-filter-preset
                  '("-work_habit" "-ignore"))))

               ("w" "Agenda and work actions"
                ((tags-todo "-study/NEXT"
                            ((org-agenda-overriding-header "Next Actions")))
                 (agenda ""
                         ((org-agenda-span 'fortnight)))
                 (todo "WAITING"
                       ((org-agenda-overriding-header "Waiting")))
                 (stuck ""
                        ((org-agenda-overriding-header "Stuck Cards")))
                 (todo "READY-TO-START"
                       ((org-agenda-overriding-header "Upcoming Cards")))
                 (tags-todo "study/NEXT"
                            ((org-agenda-overriding-header "Study"))))
                ((org-agenda-tag-filter-preset '("-ignore"))
                 (org-agenda-files (list org-work-file org-agenda-diary-file org-jira-working-dir))
                 (org-deadline-warning-days 0)
                 (org-agenda-todo-ignore-deadlines 14)
                 (org-agenda-todo-ignore-scheduled 'all)
                 (org-agenda-remove-tags t)
                 (org-stuck-projects '("-ignore+TODO={IN-PROGRESS}+assignee=\"chrisb\"/-RESOLVED-DONE" ("NEXT") nil "SCHEDULED:\\|\\<IGNORE\\>"))
                 ))

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
                  '("-drill" "-gtd" "-work_habit"))
                 (org-habit-show-habits nil)
                 (org-agenda-include-inactive-timestamps t))))))

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
      (setq org-archive-default-command 'org/archive-done-tasks)

      (defadvice org-archive-subtree
          (before add-inherited-tags-before-org-archive-subtree activate)
        "Add inherited tags before org-archive-subtree."
        (org-set-tags-to (org-get-tags-at))))))

(defun cb-org/init-org-table ()
  (use-package org-table
    :config
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org/recalculate-whole-table)))

(defun cb-org/init-org-protocol ()
  (use-package org-protocol
    :config
    (defadvice org-protocol-do-capture (after show-growl-notification activate)
      "Show Growl notification when capturing links."
      (when (equal system-type 'darwin)
        (let* ((parts (org-protocol-split-data (ad-get-arg 0) t org-protocol-data-separator))
               ;; Pop the template selector if present.
               (template (or (and (>= 2 (length (car parts))) (pop parts))
                             org-protocol-default-template-key))
               (title (or (cadr parts) ""))
               (url (org-protocol-sanitize-uri (car parts))))
          (org/growl "Link Stored" (or title url)))))))

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
      (setq org-clock-persist t)
      (setq org-clock-persist-query-resume nil)
      (setq org-clock-history-length 20)
      (setq org-clock-in-resume t)
      (setq org-clock-report-include-clocking-task t)
      (setq org-clock-in-switch-to-state 'org/clock-in-to-next-state)
      (setq org-clock-out-remove-zero-time-clocks t)

      (org-clock-persistence-insinuate)

      (add-hook 'org-clock-out-hook 'org/remove-empty-clock-drawers t))))

(defun cb-org/init-org-crypt ()
  (use-package org-crypt
    :config
    (progn
      (setq org-crypt-disable-auto-save 'encypt)
      (org-crypt-use-before-save-magic)
      (add-to-list 'org-tags-exclude-from-inheritance "crypt")
      (add-hook 'org-ctrl-c-ctrl-c-hook 'org/decrypt-entry))))

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

(defun cb-org/init-org-export ()
  (use-package org-export
    :defer t
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
      (add-hook 'org-ctrl-c-ctrl-c-hook 'org/C-c-C-c-export-koma-letter t)
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
