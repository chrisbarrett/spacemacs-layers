;;; mu4e-unread-messages.el --- Utility for testing if there is unread mail.  -*- lexical-binding: t; -*-

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

;; Provides a function for testing whether there are unread messages in a
;; maildir. The provided function is suitable for use as a value for
;; `display-time-mail-function'.

;;; Code:

(require 's)
(require 'dash)
(require 'mu4e)

(defgroup mu4e-unread-messages nil
  "Display an indicator in the modeline for unread mu4e messages."
  :group 'mail
  :prefix "mu4e-ml-")

(defcustom mu4e-unread-messages-mu-executable (executable-find "mu")
  "The path to the mu executable."
  :group 'mu4e-unread-messages
  :type 'string)

(defun mu4e-unread-messages--find-inboxes ()
  "Use find(1) to search the mu4e maildir for inboxes."
  (let* ((command (format "find '%s' -type d -name INBOX " mu4e-maildir))
         (results (shell-command-to-string command))
         (dirs (s-split "\n" results t)))
    (--map (s-chop-prefix mu4e-maildir it) dirs)))

(defcustom mu4e-unread-messages-inboxes (mu4e-unread-messages--find-inboxes)
  "The names of the maildir boxes to query for unread messages.
Example:

  (\"/INBOX\" \"/personal/inbox\")

The default implementation uses find(1) to search for \"INBOX\" in
`mu4e-maildir'."
  :group 'mu4e-unread-messages
  :type '(repeat directory))

(defun mu4e-unread-messages? ()
  "Return whether there is unread mail in any of the accounts defined by `mu4e-unread-messages-inboxes'."
  (let* ((command (format "mu find %s | wc -l" (mu4e-unread-messages--mk-query-string mu4e-unread-messages-inboxes)))
         (n-matches (s-trim (shell-command-to-string command))))
    (not (zerop (string-to-number n-matches)))))

(defun mu4e-unread-messages--mk-query-string (inboxes)
  (format "'(' %s ')' AND flag:unread"
          (s-join " OR " (--map (format "maildir:'%s'" it) inboxes))))

(provide 'mu4e-unread-messages)

;;; mu4e-unread-messages.el ends here
