;;; circe-notifications.el --- Add desktop notifications to Circe.

;; Copyright (C) 2014 - 2015 Ruben Maher

;; Author: Ruben Maher <r@rkm.id.au>
;; URL: https://code.rkm.id.au/circe-notifications

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

;;; Contributors:

;; Michael McCracken <michael.mccracken@gmail.com>

;;; Code:

(require 'circe)
(require 'cl-macs)
(require 'dbus)
(require 'notifications)
(require 's)
(require 'xml)

(defgroup circe-notifications nil
  "Add desktop notifications to Circe."
  :prefix "circe-notifications-"
  :group 'circe)

(defvar circe-notifications-wait-list nil
  "An alist of nicks that have triggered notifications in the last
`circe-notifications-wait-for' seconds.")

(defvar circe-notifications-nicks-on-all-networks nil
  "A list of nicks in use according to `circe-network-options'.  It is generated
by `circe-notifications-get-nicks-on-all-networks'.")

(defcustom circe-notifications-growlnotify-command
  (executable-find "growlnotify")
  "The path to growlnotify.  For OSX users.")

(defcustom circe-notifications-osascript-command
  (executable-find "osascript")
  "The path to osascript.  For OSX users.")

(defcustom circe-notifications-terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.  For OSX users.")

(defcustom circe-notifications-backend
  (or (if (string-equal system-type "darwin") "osascript") "dbus")
  "Which notifications backend to use.  One of `dbus', `growlnotify',
`osascript' or `terminal-notifier'."
  :type '(choice
          (const :tag "Use dbus" "dbus")
          (const :tag "Use growlnotify" "growlnotify")
          (const :tag "Use osascript" "osascript")
          (const :tag "Use terminal-notifier" "terminal-notifier"))
  :group 'circe-notifications)

(defcustom circe-notifications-wait-for 90
  "The number of seconds to wait before allowing some nick in
`circe-notifications-wait-list' to trigger a notification again."
  :type 'integer
  :group 'circe-notifications)

(defcustom circe-notifications-watch-strings nil
  "A list of strings which can trigger a notification.  You don't need to put
your nick here, it is added automagically by
`circe-notifications-nicks-on-all-networks' when it checks the values in
`circe-network-options'."
  :type '(repeat string)
  :group 'circe-notifications)

(defcustom circe-notifications-check-window-focus t
  "Enable use of external tools to check if Emacs is focused by the window
manager.  Tries to use xprop and xdotool if they are available, or if
`system-type' is \"darwin\" it will use `circe-notifications-osascript-command'.

xprop and xdotool may detect Emacs running in a terminal emulator as long as
that terminal emulator is capable of setting WM_NAME and
`circe-notifications-term-name' is set."
  :type 'boolean
  :group 'circe-notifications)

(defcustom circe-notifications-term-name "xterm"
  "The name of the terminal you will run Emacs in (if any).  If you are unsure
check the value of WM_CLASS with xprop.  This is necessary to prevent false
positives."
  :type 'string
  :group 'circe-notifications)

(defcustom circe-notifications-desktop-entry
  (concat "emacs" (int-to-string emacs-major-version))
  "Icon to use for notifications."
  :type 'file
  :group 'circe-notifications)

(defcustom circe-notifications-sound-name "message-new-entry"
  "Sound to use for notifications."
  :type 'file
  :group 'circe-notifications)

(defcustom circe-notifications-timeout 1000
  "How long the notification should be displayed for (milliseconds)."
  :type 'integer
  :group 'circe-notifications)

(defun circe-notifications-PRIVMSG (nick userhost _command target text)
  "If NICK is not in either `circe-ignore-list' or `circe-fool-list' (only
applicable if `lui-fools-hidden-p'), NICK or TARGET is either in
`tracking-buffers' \(i.e., not currently visible) or Emacs is not currently
focused by the window manager (detected if
`circe-notifications-check-window-focus' is true), NICK has not triggered a
notification in the last `circe-notifications-wait-for' seconds and NICK or TEXT
matches any of `circe-notifications-nicks-on-all-networks' or
`circe-notifications-watch-strings', show a desktop notification."
  (unless (cond ((member nick circe-ignore-list))
                ((and (member nick circe-fool-list)
                      (lui-fools-hidden-p))))
    ;; Checking `tracking-buffers' has the benefit of excluding
    ;; `tracking-ignored-buffers'.  Also if a channel is in `tracking-buffers',
    ;; it is not currently focused by Emacs.
    (when (cond ((member nick tracking-buffers))   ; Private message
                ((member target tracking-buffers)) ; Message to a channel
                ((not (circe-notifications-emacs-focused-p))))
      (when (circe-notifications-not-getting-spammed-by nick)
        (if (cond ((dolist (n circe-notifications-nicks-on-all-networks)
                       (if (or (string-match n target)  ; Private message
                               (string-match n text))   ; Message to a channel
                             (cl-return t))))
                    ((dolist (n circe-notifications-watch-strings)
                       (if (or (string-match n nick)
                               (string-match n text))
                             (cl-return t)))))
                     (circe-notifications-notify nick text target))))))

(defun circe-notifications-JOIN (nick userhost _command channel
                                      &optional accountname realname)
  "If NICK is not in either `circe-ignore-list' or `circe-fool-list' (only
applicable if `lui-fools-hidden-p'), CHANNEL is either in `tracking-buffers'
\(i.e., not currently visible) or Emacs is not currently focused by the window
manager (detected if `circe-notifications-check-window-focus' is true), NICK has
not triggered a notification in the last `circe-notifications-wait-for' seconds
and NICK matches any of `circe-notifications-watch-strings', show a desktop
notification."
  (unless (cond ((member nick circe-ignore-list))
                ((and (member nick circe-fool-list)
                      (lui-fools-hidden-p))))
    (when (cond ((member channel tracking-buffers))
                ((not (circe-notifications-emacs-focused-p))))
      (when (circe-notifications-not-getting-spammed-by nick)
        (when (dolist (n circe-notifications-watch-strings)
                       (if (string-match n nick) (cl-return t)))
          (circe-notifications-notify
           nick (concat "/JOIN " channel) channel))))))

(defun circe-notifications-QUIT (nick userhost _command
                                      &optional channel reason)
  "If NICK is not in either `circe-ignore-list' or `circe-fool-list' (only
applicable if `lui-fools-hidden-p'), CHANNEL is either in `tracking-buffers'
\(i.e., not currently visible) or Emacs is not currently focused by the window
manager (detected if `circe-notifications-check-window-focus' is true), NICK has
not triggered a notification in the last `circe-notifications-wait-for' seconds
and NICK matches any of `circe-notifications-watch-strings', show a desktop
notification."
  (unless (cond ((member nick circe-ignore-list))
                ((and (member nick circe-fool-list)
                      (lui-fools-hidden-p))))
    (when (cond ((and channel (member channel tracking-buffers)))
                ((not (circe-notifications-emacs-focused-p))))
      (when (circe-notifications-not-getting-spammed-by nick)
        (when (dolist (n circe-notifications-watch-strings)
                       (if (string-match n nick) (cl-return t)))
          (circe-notifications-notify nick "/QUIT" (or channel "")))))))

(defun circe-notifications-PART (nick userhost _command channel
                                      &optional reason)
  "If NICK is not in either `circe-ignore-list' or `circe-fool-list' (only
applicable if `lui-fools-hidden-p'), CHANNEL is either in `tracking-buffers'
\(i.e., not currently visible) or Emacs is not currently focused by the window
manager (detected if `circe-notifications-check-window-focus' is true), NICK has
not triggered a notification in the last `circe-notifications-wait-for' seconds
and NICK matches any of `circe-notifications-watch-strings', show a desktop
notification."
  (unless (cond ((member nick circe-ignore-list))
                ((and (member nick circe-fool-list)
                      (lui-fools-hidden-p))))
    (when (cond ((member channel tracking-buffers))
                ((not (circe-notifications-emacs-focused-p))))
      (when (circe-notifications-not-getting-spammed-by nick)
        (when (dolist (n circe-notifications-watch-strings)
                       (if (string-match n nick) (cl-return t)))
          (circe-notifications-notify
           nick (concat "/PART (" channel ")") (or channel "")))))))

(defun circe-notifications-notify (nick body &optional channel)
  "Show a desktop notification from NICK with BODY."
  (cond ((string-equal circe-notifications-backend "dbus")
         (dbus-ignore-errors
           (notifications-notify
            :title (xml-escape-string nick)
            :body (xml-escape-string body)
            :timeout circe-notifications-timeout
            :desktop-entry circe-notifications-desktop-entry
            :sound-name circe-notifications-sound-name
            :transient)))
        ((string-equal circe-notifications-backend "growlnotify")
         (let* ((process
                 (start-process
                  "growlnotify" nil
                  circe-notifications-growlnotify-command
                  (encode-coding-string
                   (xml-escape-string nick)
                   (keyboard-coding-system))
                  "-a" "Emacs"
                  "-n" "Circe IRC")))
           (process-send-string
            process (encode-coding-string
                     (xml-escape-string body)
                     (keyboard-coding-system)))
           (process-send-string process "\n")
           (process-send-eof process)))
        ((string-equal circe-notifications-backend "osascript")
         (start-process
          "osascript" nil
          circe-notifications-osascript-command
          "-e"
          (format
           (concat "display notification \"%s\" with"
                   " title \"%s\" subtitle \"%s\"")
                  (encode-coding-string
                   (xml-escape-string body)
                   (keyboard-coding-system))
                  (encode-coding-string
                   (xml-escape-string nick)
                   (keyboard-coding-system))
                  (encode-coding-string
                   (xml-escape-string channel)
                   (keyboard-coding-system)))))
        ((string-equal circe-notifications-backend "terminal-notifier")
         (start-process "terminal-notifier" nil
                        circe-notifications-terminal-notifier-command
                        "-title" (xml-escape-string nick)
                        "-message" (xml-escape-string body)
                        "-activate" "org.gnu.Emacs"))
        (t nil)))

(defun circe-notifications-not-getting-spammed-by (nick)
  "Return an alist with NICKs that have triggered notifications in the last
`circe-notifications-wait-for' seconds, or nil if it has been less than
`circe-notifications-wait-for' seconds since the last notification from NICK."
  (if (assoc nick circe-notifications-wait-list)
      (circe-notifications-wait-a-bit nick)
    (add-to-list 'circe-notifications-wait-list
                 (cons nick (float-time)))))

(defun circe-notifications-wait-a-bit (nick)
  "Check if it has been more than `circe-notifications-wait-for' seconds since
the last message from NICK.  If so, remove them from
`circe-notifications-wait-list'."
  (let* ((last-time (assoc-default
                     nick
                     circe-notifications-wait-list
                     (lambda (x y)
                       (string-match y x))))
         (seconds-since (- (float-time) last-time)))
    (when (< circe-notifications-wait-for seconds-since)
      (progn (setq circe-notifications-wait-list
                   (delq
                    (assoc nick circe-notifications-wait-list)
                    circe-notifications-wait-list))
             ;; add NICK to the waitlist again
             (circe-notifications-not-getting-spammed-by nick)))))

(defun circe-notifications-get-nicks-on-all-networks ()
  "Get a list of all nicks in use according to `circe-network-options'."
  (let ((x 0))
    (while (< x (length circe-network-options))
      (let ((it (nth 2 (nth x circe-network-options))))
        (when (stringp it)
          (add-to-list 'circe-notifications-nicks-on-all-networks it))
        (cl-incf x)))))


(defun circe-notifications-has-x-tools-p ()
  "True if $DISPLAY is set and both xdotool and xprop are installed."
  (if (and (< 0 (string-width (shell-command-to-string "echo $DISPLAY")))
           (executable-find "xdotool")
           (executable-find "xprop"))
      t
    nil))

(defun circe-notifications-emacs-focused-p ()
  "True if Emacs is focused by the window manager."
  (when circe-notifications-check-window-focus
    (cond ((circe-notifications-has-x-tools-p)
           (let* ((focused-window
                   (shell-command-to-string "xdotool getwindowfocus"))
                  (window-class (shell-command-to-string
                                 (concat "xprop WM_CLASS -id " focused-window)))
                  (window-name (shell-command-to-string
                                (concat "xprop WM_NAME -id " focused-window))))
             (if (string-match "emacs" window-class)
                 t
               (if (and circe-notifications-term-name
                        (string-match circe-notifications-term-name
                                      window-class)
                        (string-match "emacs" window-name))
                   t
                 nil))))
          ((string-equal system-type "darwin")
           (if (s-starts-with?
                "Emacs"
                (s-trim
                 (shell-command-to-string
                  (concat
                   circe-notifications-osascript-command
                   " -e 'tell application \"System Events\"' -e 'set"
                   " frontApp to name of first application process whose"
                   " frontmost is true' -e 'end tell'"))) t)
               t
             nil)))))

(defun enable-circe-notifications ()
  "Turn on notifications."
  (interactive)
  (circe-notifications-get-nicks-on-all-networks)
  (advice-add 'circe-display-PRIVMSG :after 'circe-notifications-PRIVMSG)
  (advice-add 'circe-display-channel-quit :after 'circe-notifications-QUIT)
  (advice-add 'circe-display-JOIN :after 'circe-notifications-JOIN)
  (advice-add 'circe-display-PART :after 'circe-notifications-PART))

(defun disable-circe-notifications ()
  "Turn on notifications."
  (interactive)
  (advice-remove 'circe-display-PRIVMSG 'circe-notifications-PRIVMSG)
  (advice-remove 'circe-display-channel-quit 'circe-notifications-QUIT)
  (advice-remove 'circe-display-JOIN 'circe-notifications-JOIN)
  (advice-remove 'circe-display-PART 'circe-notifications-PART))

(provide 'circe-notifications)
;;; circe-notifications.el ends here
