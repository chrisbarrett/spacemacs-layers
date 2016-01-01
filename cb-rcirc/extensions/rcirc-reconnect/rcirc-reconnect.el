;;; rcirc-reconnect.el --- automatically reconnect rcirc -*- lexical-binding: t; -*-

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
;;
;; Automatically reconnect rcirc if connection is broken. From emacswiki:
;;
;;     http://www.emacswiki.org/emacs/rcircReconnect
;;
;; Attempt reconnection at increasing intervals when a connection is lost.

;;; Code:

(require 'rcirc)
(require 'cl-lib)
(require 'dash)

(defvar rcirc-reconnect--rcirc-sender "rcirc-reconnect.el")

;;; Counter used for tracking time til next reconnection attempt.

(defvar-local rcirc-reconnect-attempts 0)

(defun rcirc-reconnect--increment-reconnection-counter (process)
  (with-current-buffer (process-buffer process)
    (cl-incf rcirc-reconnect-attempts)))

(defun rcirc-reconnect--reset-reconnection-counter (process)
  (with-current-buffer (process-buffer process)
    (setq rcirc-reconnect-attempts 0)))

(defun rcirc-reconnect--time-to-next-reconnection (process)
  (with-current-buffer (process-buffer process)
    (exp (1+ rcirc-reconnect-attempts))))

;;; Reconnection implementation.

;;;###autoload
(define-minor-mode rcirc-reconnect-mode
  "Automatically reconnect an RCIRC session if the connection is lost."
  nil " Auto-Reconnect" nil
  (if rcirc-reconnect-mode
      (add-hook 'rcirc-sentinel-functions 'rcirc-reconnect--schedule nil t)
    (remove-hook 'rcirc-sentinel-functions 'rcirc-reconnect--schedule t)))

(defun rcirc-reconnect--process-disconnected-p (process)
  (and (eq 'closed (process-status process))
       (buffer-live-p (process-buffer process))))

(defun rcirc-reconnect--perform-reconnect (process)
  (when (rcirc-reconnect--process-disconnected-p process)
    (with-rcirc-process-buffer process
      (when rcirc-reconnect-mode
        (if (process-live-p process) ; user reconnected manually
            (rcirc-reconnect--reset-reconnection-counter process)
          (rcirc-reconnect--log-error process "Attempting reconnect to %s..." (process-name process))
          (rcirc-reconnect--attempt-or-schedule-reconnection process))))))

(defun rcirc-reconnect--rcirc-server-name ()
  (with-current-buffer (rcirc-reconnect--server-buffer)
    (or rcirc-server
        (when (boundp 'rcirc-default-server) rcirc-default-server))))

(defun rcirc-reconnect--reconnecting-from-server-buffer-p ()
  (equal (current-buffer) (rcirc-reconnect--server-buffer)))

(defun rcirc-reconnect--channels-for-reconnection ()
  (when (rcirc-reconnect--reconnecting-from-server-buffer-p)
    (with-current-buffer (rcirc-reconnect--server-buffer)
      (-when-let* ((server (rcirc-reconnect--rcirc-server-name))
                   ((_ . server-plist) (assoc-string server rcirc-server-alist)))
        (plist-get server-plist :channels)))))

(defun rcirc-reconnect--suppress-channel-parting ()
  (when (rcirc-reconnect--reconnecting-from-server-buffer-p)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-hook 'change-major-mode-hook 'rcirc-change-major-mode-hook)))))

(defun rcirc-reconnect--kill-client-process ()
  (-when-let (process (get-buffer-process (rcirc-reconnect--server-buffer)))
    (delete-process process)))

(defun rcirc-reconnect--server-buffer ()
  (or rcirc-server-buffer
      (-when-let* ((default-server) (-map 'car rcirc-server-alist)
                   (buf (get-buffer (format "*%s*" default-server))))
        (when (derived-mode-p 'rcirc-mode buf)
          buf))))

;; rcirc-cmd-reconnect
(defun-rcirc-command reconnect (&optional _arg)
  "Reconnect the server process."
  (interactive "i")
  (save-excursion
    (when (buffer-live-p (rcirc-reconnect--server-buffer))
      (rcirc-reconnect--suppress-channel-parting)
      (rcirc-reconnect--kill-client-process))
    (rcirc-connect (rcirc-reconnect--rcirc-server-name)
                   (if (boundp 'rcirc-port) rcirc-port rcirc-default-port)
                   (or rcirc-nick rcirc-default-nick)
                   nil
                   nil
                   (rcirc-reconnect--channels-for-reconnection))))

(defun rcirc-reconnect--attempt-or-schedule-reconnection (process)
  (condition-case err
      (progn
        (save-window-excursion (save-excursion (rcirc-cmd-reconnect)))
        (rcirc-reconnect--reset-reconnection-counter process))
    ((quit error)
     (rcirc-reconnect--log-error process "Reconnection attempt failed: %s" err)
     (rcirc-reconnect--increment-reconnection-counter process)
     (rcirc-reconnect--schedule process))))

(defun rcirc-reconnect--schedule (process &optional _sentinel)
  (let ((reconnect-delay (rcirc-reconnect--time-to-next-reconnection process)))
    (condition-case err
        (when (rcirc-reconnect--process-disconnected-p process)
          (with-rcirc-process-buffer process
            (rcirc-reconnect--log-error process "Scheduling reconnection attempt in %s second(s)." reconnect-delay)
            (run-with-timer reconnect-delay nil 'rcirc-reconnect--perform-reconnect process)))
      (error
       (rcirc-reconnect--log-error process "%S" err)))))

(defun rcirc-reconnect--log-error (process fmt-string &rest args)
  (rcirc-print process
               rcirc-reconnect--rcirc-sender
               "ERROR"
               rcirc-target
               (apply 'format fmt-string args)
               t))

(provide 'rcirc-reconnect)

;;; rcirc-reconnect.el ends here
