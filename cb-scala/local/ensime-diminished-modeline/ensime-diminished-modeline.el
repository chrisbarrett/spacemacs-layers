;;; ensime-diminished-modeline.el --- More compact modeline for ENSIME.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

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

(require 'ensime)

(defun ensime-diminished--pretty-error-counts (conn)
  (let* ((errors (ensime-num-errors conn))
         (warnings (ensime-num-warnings conn))
         (parts (list (when (plusp errors)
                        (propertize (format "•%s" errors) 'face 'flycheck-fringe-error))
                      (when (plusp warnings)
                        (propertize (format "•%s" warnings) 'face 'flycheck-fringe-warning)))))
    (when (plusp (+ errors warnings))
      (concat " " (s-join " " (-non-nil parts))))))

(with-eval-after-load 'ensime

  (defun ensime-modeline-string ()
    "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
    (when ensime-mode
      (condition-case _
          (let ((conn (ensime-connection-or-nil)))
            (cond ((and ensime-mode (not conn))
                   (cond
                    ((ensime-owning-server-process-for-source-file buffer-file-name)
                     " Ⓔ[...]")
                    (t " Ⓔ[-/-]")))

                  ((and ensime-mode (ensime-connected-p conn))
                   (concat " "
                           (or (plist-get (ensime-config conn) :name)
                               "Ⓔ[ ✓ ]")
                           (let ((status (ensime-modeline-state-string conn))
                                 (unready (not (ensime-analyzer-ready conn))))
                             (cond (status (concat "[" status "]"))
                                   (unready "[>>>]")
                                   (t "")))
                           (ensime-diminished--pretty-error-counts conn)))
                  (ensime-mode " Ⓔ[Dead]")
                  ))
        (error (progn
                 " Ⓔ[wtf]"))))))

(provide 'ensime-diminished-modeline)

;;; ensime-diminished-modeline.el ends here
