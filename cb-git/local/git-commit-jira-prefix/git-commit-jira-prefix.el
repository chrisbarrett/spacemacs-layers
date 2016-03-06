;;; git-commit-jira-prefix.el --- Prefix commit messages with JIRA ticket numbers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or modify
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

;; Extract the JIRA ticket number from the current branch name, and prepend
;; commit messages with that ticket number.

;;; Code:

(require 'dash)
(require 's)
(require 'git-commit)

(autoload 'magit-get-current-branch "magit-git")

(defun git-commit-jira-prefix--ticket-number ()
  (-when-let* ((branch (magit-get-current-branch))
               ((ticket) (s-match (rx bos (group (+ alpha) "-" (+ digit))) branch)))
    ticket))

;;;###autoload
(defun git-commit-jira-prefix-insert-ticket-number ()
  "Insert the JIRA ticket number, unless it's already in the buffer."
  (interactive)
  (-when-let (ticket (git-commit-jira-prefix--ticket-number))
    (goto-char (point-min))
    (unless (s-contains? ticket (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
      (goto-char (line-beginning-position))
      (insert (format "%s:" ticket))
      (just-one-space))))

;;;###autoload
(defun git-commit-jira-prefix-insert ()
  (let ((buf (current-buffer)))
    ;; Run after `server-execute', which is run using
    ;; a timer which starts immediately.
    (run-with-timer
     0.01 nil (lambda ()
                (with-current-buffer buf
                  (git-commit-jira-prefix-insert-ticket-number)
                  (goto-char (line-end-position)))))))

;;;###autoload
(defun git-commit-jira-prefix-init ()
  (add-hook 'git-commit-setup-hook #'git-commit-jira-prefix-insert)
  (define-key git-commit-mode-map (kbd "C-c C-a") #'git-commit-jira-prefix-insert-ticket-number))

(provide 'git-commit-jira-prefix)

;;; git-commit-jira-prefix.el ends here
