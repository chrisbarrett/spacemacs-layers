;;; org-work.el --- Configure orgmode for use as a work management suite.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((f "0.17") (s "1.9") (dash "2.8") (cl-lib "0.3") (emacs "24.3"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure orgmode for use as a work management suite.

;;; Code:

;; HACK: Fix flycheck load path for Cask.
(eval-and-compile
  (dolist (x (file-expand-wildcards "./.cask/**/**/**"))
    (add-to-list 'load-path x)))

(require 'org)
(require 'org-agenda)
(require 's)
(require 'f)
(require 'dash)
(require 'cl-lib)

;;; Customisable vars

(defvar org-work-project-subheadings
  '("Dates" "Tasks")
  "Lists the titles of subheadings to create under each new project.")

(defvar org-work-file (f-join org-directory "work.org")
  "Defines the path to file for work-related todos, etc.")

(defvar org-work-persistence-file (f-join org-directory ".org-at-work")
  "File to create that saves work state between sessions.")

;;; String utilities

(defun org-work--s-acronymise (str)
  "Transform STR to an acronym according to its constituent words."
  (s-join "" (--map (s-upcase (substring it 0 1)) (s-split-words str))))

(defun org-work--s-abbreviate (str)
  "Abbreviate STR, either by CamelCasing or acronymising."
  (if (< 9 (length str))
      (org-work--s-acronymise str)
    (s-upper-camel-case str)))

(defun org-work--timestamp ()
  "Return an inactive timestamp with HH:MM."
  (with-temp-buffer
    (org-insert-time-stamp nil t t)
    (buffer-string)))

;;; Search utilities

(defun org-work--projects ()
  "Return a list of projects defined in the current buffer."
  (save-restriction
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (search-forward-regexp (rx bol "*" (+ space) "Projects") nil t)
        (-sort 'string<
               (org-work--map-childern
                (lambda ()
                  (substring-no-properties (org-get-heading t t)))))))))

(defun org-work--upwards-collect-headings ()
  "Move backwards and up headings from point, collecting the heading names."
  (save-excursion
    (nreverse (cl-loop collecting (substring-no-properties (org-get-heading t t))
                       while (org-up-heading-safe)))))

(defun org-work--project-at-point ()
  "Return the name of the project at point, or nil if not at a project.
A project is a subheading of the Projects toplevel heading."
  (save-restriction
    (widen)
    (cl-destructuring-bind
        (&optional top subhead &rest hs) (org-work--upwards-collect-headings)
      (and (equal "Projects" top) subhead))))

;;; Project navigation

(defvar org-work--last-project nil
  "The last project read from the user.")

(defun org-work--read-project (&optional prompt)
  "Read a project name from the user, displaying PROMPT.
The last project read from the user will come first."
  (let* ((ps (org-work--projects))
         ;; Re-order the projects list so the last one used goes to the head of
         ;; the list.
         (ordered
          (if (and org-work--last-project
                   (-contains? ps org-work--last-project))
              (cons org-work--last-project (-difference ps (list org-work--last-project)))
            ps))
         (result (ido-completing-read (or prompt
                                          "Project (create if not matching): ")
                                      ordered)))
    (setq org-work--last-project result)
    result))

;;; Project creation

(defun org-work--goto-create-heading (title)
  "Go to or create a toplevel heading with TITLE."
  (goto-char (point-min))
  (unless (search-forward-regexp (eval `(rx bol "*" (+ space) ,title))
                                 nil t)
    (goto-char (point-max))
    (newline)
    (insert (concat "* " title))))

(defun org-work--goto-projects ()
  "Move point to the Projects heading, creating it if needed."
  (widen)
  (org-work--goto-create-heading "Projects"))

(defun org-work-create-project (name)
  "Create a new project with NAME in the current buffer."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)))

  ;; Insert heading.
  (org-work--goto-projects)
  (org-insert-subheading nil)
  (insert name)

  ;; Insert subheadings
  (goto-char (line-end-position))
  (let* ((level (org-current-level))
         (stars (s-repeat (1+ level) "*"))
         (str (->> org-work-project-subheadings
                (--map (format "%s %s" stars it))
                (s-join "\n")))
         )
    (insert (concat "\n" str)))

  (org-up-heading-safe)

  ;; Set properties.
  (org-set-property "CATEGORY" (org-work--s-abbreviate name))
  (org-set-property "CREATED" (org-work--timestamp)))

(defun org-work--map-childern (fn)
  "Call FN on every child heading of the current.
Return a list of the function's results."
  (save-excursion
    (org-back-to-heading)
    (let ((acc)
          (parent-level (org-current-level)))

      (outline-next-heading)
      (let ((child-level (org-current-level)))
        ;; If the next heading is not a child, there are no child headings.
        (unless (equal parent-level child-level)
          (push (funcall fn) acc)
          ;; Continue to walk headings at this level.
          (while (let ((start (point)))
                   (org-forward-heading-same-level nil t)
                   (not (equal start (point))))
            (push (funcall fn) acc))))

      (nreverse acc))))

(defun org-work-goto-create-project (name)
  "Go to project with heading NAME.
Interactively create a new project if no matching project found."
  (interactive (list (org-work--read-project)))

  (widen)
  (org-work--goto-projects)
  ;; Get first headline matching title.
  (let ((pos
         (->> (org-work--map-childern
               (lambda () (when (equal name (org-get-heading t t)) (point))))
           -non-nil
           car)))
    (cond
     (pos
      (goto-char pos))

     ((y-or-n-p "Create new project? ")
      (atomic-change-group
        (org-work-create-project name)
        ;; Re-sort projects alphabetically.
        (save-excursion
          (org-work--goto-projects)
          (org-sort-entries nil ?a))
        ;; Show the newly-created project.
        (org-work-goto-create-project name)))

     (t
      (error "Aborted")))

    (org-narrow-to-subtree)
    (show-entry)
    (show-children)))

;;; Task creation

(defun org-work--goto-create-tasks-subheading ()
  "Move to the tasks subheading of the current project.
Create the heading if it does not exist."
  (unless (search-forward-regexp (rx bol (+ "*") (+ space) "Tasks")
                                 nil t)
    (goto-char (point-min))
    (org-insert-heading-respect-content)
    (org-demote)
    (insert "Tasks")
    (org-set-tags-to ":task:")))

(defun org-work-add-task (title project)
  "Add a task with TITLE to PROJECT."
  (interactive
   (list
    (read-string "Task title: ")
    (or (org-work--project-at-point)
        (org-work--read-project))))

  (org-work-goto-create-project project)
  (org-work--goto-create-tasks-subheading)

  ;; Insert subheading
  (org-insert-heading-respect-content)
  (org-demote)
  (insert (concat "TODO " title))
  (goto-char (line-end-position))
  (newline))

;;; Clocking

(defvar org-work--at-work? nil
  "Non-nil if currently 'at work'.")

(defvar org-work-started-work-hook nil
  "Hook run after starting work.")

(defvar org-work-left-work-hook nil
  "Hook run after leaving work.")

(defvar org-work-state-changed-hook nil
  "Hook run after starting or leaving work.")

(defun org-work--goto-or-create-work-log ()
  "Move to today's date log entry, creating it if necessary."
  (cl-destructuring-bind (_s _m _h d mo y &rest _)
      (decode-time (current-time))
    (org-datetree-find-date-create (list mo d y))))

(defun org-work-start-work (file)
  "Start work, with FILE as the project file."
  (with-current-buffer (find-file-noselect file)
    (org-work--goto-or-create-work-log)
    (org-clock-in))
  (setq org-work--at-work? t)
  (f-touch org-work-persistence-file)
  (run-hooks 'org-work-started-work-hook)
  (message "Started work"))

(defun org-work-leave-work (file)
  "Leave work, with FILE as the project file."
  (with-current-buffer (find-file-noselect file)
    (org-work--goto-or-create-work-log)
    (org-clock-out nil t))
  (setq org-work--at-work? nil)
  (when (f-exists? org-work-persistence-file)
    (f-delete org-work-persistence-file))

  (run-hooks 'org-work-left-work-hook)
  (message "Left work"))

(defun org-work-toggle-at-work (file)
  "Toggle whether I am currently at work.  FILE is the project file."
  (interactive (list org-work-file))
  (if org-work--at-work?
      (org-work-leave-work file)
    (org-work-start-work file))
  (run-hooks 'org-work-state-changed-hook))

(defun org-work-maybe-start-work ()
  "Set status to 'at-work' if the work persistence file exists."
  (when (f-exists? org-work-persistence-file)
    (org-work-start-work org-work-file)))

;;; Minor mode

;;;###autoload
(defvar org-work-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-p") 'org-work-goto-create-project)
    (define-key km (kbd "C-c C-n") 'org-work-add-task)
    km)
  "Keymap for `org-work-mode'.")

;;;###autoload
(define-minor-mode org-work-mode
  "Minor mode for work-related org buffers."
  nil
  " Work"
  org-work-mode-map)

(defun maybe-enable-org-work-mode ()
  "Enable org-work-mode for ORG-WORK org buffers."
  (when (s-matches? (rx "work" (* anything) "org" eol)
                    (buffer-name))
    (org-work-mode +1)))

(add-hook 'org-mode-hook 'maybe-enable-org-work-mode)

(provide 'org-work)

;;; org-work.el ends here
