;;; cb-exit-emacs.el --- Commands for exiting Emacs.  -*- lexical-binding: t; -*-

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

;;; Code:

(autoload 'server-done "server")

;;;###autoload
(defun cb-exit-emacs ()
  "Close the current server frame or exit Emacs."
  (interactive)
  (let* ((emacsclient-frame? (and (not (display-graphic-p))
                                  (< 1 (length (frame-list)))))
         (prompt (if emacsclient-frame? "Finish editing? " "Kill Emacs? ")))
    (when (yes-or-no-p prompt)
      (cond
       (emacsclient-frame?
        (server-done))
       ((daemonp)
        (server-save-buffers-kill-terminal nil))
       (t
        (save-buffers-kill-emacs))))))

;;;###autoload
(defun cb-exit-emacs-warn-rebound ()
  "Show an error that the Emacs exit key has been rebound."
  (interactive)
  (user-error "Type <C-c k k> to exit Emacs"))

(provide 'cb-exit-emacs)

;;; cb-exit-emacs.el ends here
