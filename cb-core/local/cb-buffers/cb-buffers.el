;;; cb-buffers.el --- Buffer utilities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((s "1.10.0") (dash "2.12.1"))
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

;; Utilities and commands for buffer interaction.

;;; Code:

(require 'dash)

(defgroup cb-buffers nil
  "Functions for working with buffers."
  :group 'editing
  :prefix "cb-buffers-")

(defcustom cb-buffers-kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*" "*elfeed-search*"
    "work_movio.org" "diary.org" "notes.org" "*spacemacs*" " *mu4e-main*")
  "List of buffers to always bury instead of killing."
  :type '(list string))

(defcustom cb-buffers-kill-buffer-if-no-proc-list
  '("*shell*" "*eshell*" "*ansi-term*")
  "List of buffers to always kill if they do not have an associated process."
  :type '(list string))

(defcustom cb-buffers-kill-buffer-ignored-modes
  '(circe-mode)
  "List of modes whose buffers should be buried and not killed."
  :type '(list symbol))

(cl-defmacro cb-buffers-filtera (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric buffer filtering macro.

Filter over the buffers, calling PRED-FORM with each buffer
set to current.

Filters over BUFS, or the buffer list if unspecified."
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(defun cb-buffers-maybe-kill ()
  "Kill or bury the current buffer."
  (interactive)
  (if (cb-buffers--buffer-ignored-or-live? (current-buffer))
      (bury-buffer)
    ;; HACK: Avoid read-only text property errors.
    (let ((inhibit-read-only t))
      (kill-buffer (current-buffer)))))

(defun cb-buffers-maybe-kill-all ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (let ((other-buffers (-difference (buffer-list) (list (current-buffer)))))
    (dolist (buf other-buffers)
      (with-current-buffer buf
        (cb-buffers-maybe-kill)))))

(defun cb-buffers--buffer-ignored-or-live? (buf)
  (or (-contains? (-concat cb-buffers-kill-buffer-if-no-proc-list
                           cb-buffers-kill-buffer-ignored-list)
                  (buffer-name buf))
      (apply #'derived-mode-p cb-buffers-kill-buffer-ignored-modes)
      (-when-let (proc (get-buffer-process buf))
        (process-live-p proc))))

(provide 'cb-buffers)

;;; cb-buffers.el ends here
