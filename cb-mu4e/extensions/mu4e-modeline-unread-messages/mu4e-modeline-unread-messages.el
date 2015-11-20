;;; mu4e-modeline-unread-messages.el --- Display an unread indicator in the modeline.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

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

;; Modifies the global mode string to display an indicator when you have unread
;; messages in the modeline.
;;
;; Adapted from the implementation at:
;;
;;   http://writequit.org/org/settings.html

;;; Code:

(require 's)
(require 'dash)
(require 'mu4e)

(defgroup mu4e-modeline-unread-messages nil
  "Display an indicator in the modeline for unread mu4e messages."
  :group 'mail
  :prefix "mu4e-ml-")

(defcustom mu4e-ml-mu-executable (executable-find "mu")
  "The path to the mu executable."
  :group 'mu4e-modeline-unread-messages
  :type 'string)

(defcustom mu4e-ml-modeline-indicator "[NEW MAIL] "
  "The indicator to show in the modeline when there is unread mail."
  :group 'mu4e-modeline-unread-messages
  :type 'string)

(defun mu4e-ml--find-inboxes ()
  "Use find(1) to search the mu4e maildir for inboxes."
  (let* ((command (format "find '%s' -type d -name INBOX " mu4e-maildir))
         (results (shell-command-to-string command))
         (dirs (s-split "\n" results t)))
    (--map (s-chop-prefix mu4e-maildir it) dirs)))

(defcustom mu4e-ml-inboxes (mu4e-ml--find-inboxes)
  "The names of the maildir boxes to query for unread messages.
Example:

  (\"/INBOX\" \"/personal/inbox\")

The default implementation uses find(1) to search for \"INBOX\" in
`mu4e-maildir'."
  :group 'mu4e-modeline-unread-messages
  :type '(repeat directory))

;;; Public functions

;;;###autoload
(defun mu4e-modeline-unread-messages-init ()
  "Add a mu4e unread messages indicator to the modeline."
  (when (null mu4e-ml-mu-executable)
    (user-error "mu4e-modeline" "mu4e-ml-mu-executable not set"))
  (when (not (file-exists-p mu4e-ml-mu-executable))
    (user-error "mu4e-modeline" "mu4e-ml-mu-executable does not exist at specified path"))

  (setq global-mode-string
        (if (string-match-p "mu4e-ml--unread-messages?" (prin1-to-string global-mode-string))
            global-mode-string
          (cons '(mu4e-ml--unread-messages? mu4e-ml-modeline-indicator) global-mode-string)))

  (add-hook 'mu4e-index-updated-hook 'mu4e-ml--update-unread-messages-flag))

;;; Internals

(defvar mu4e-ml--unread-messages? nil)

(defvar mu4e-ml--notify-timer (run-with-timer 0 500 'mu4e-ml--update-unread-messages-flag))

(defun mu4e-ml--new-mail? ()
  "Return whether there is unread mail in any of the accounts defined by `mu4e-ml-inboxes'."
  (let* ((command (format "mu find %s | wc -l" (mu4e-ml--mk-query-string mu4e-ml-inboxes)))
         (n-matches (s-trim (shell-command-to-string command))))
    (not (zerop (string-to-number n-matches)))))

(defun mu4e-ml--mk-query-string (inboxes)
  (format "'(' %s ')' AND flag:unread"
          (s-join " OR " (--map (format "maildir:'%s'" it) inboxes))))

(defadvice mu4e-mark-execute-all (after mu4e-mark-execute-all-notify activate)
  "Update the unread mail icon after applying marks in mu4e."
  (mu4e-ml--update-unread-messages-flag))

(defun mu4e-ml--update-unread-messages-flag ()
  (let ((fn (lambda () (setq mu4e-ml--unread-messages? (mu4e-ml--new-mail?)))))
    ;; Add a delay to allow Emacs and mu to perform updates to the mail index.
    (run-with-idle-timer 1 nil fn)))

(provide 'mu4e-modeline-unread-messages)

;;; mu4e-modeline-unread-messages.el ends here
