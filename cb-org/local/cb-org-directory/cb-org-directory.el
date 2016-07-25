;;; cb-org-directory.el --- Calculate the org directory.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((f "0.17.2"))

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

(require 'f)

;;;###autoload
(defun cb-org-directory ()
  (let* ((dropbox (if (boundp 'user-dropbox-directory) user-dropbox-directory "~/Dropbox/"))
         (in-dropbox (f-join dropbox "org/")))
    (if (f-exists? in-dropbox)
        in-dropbox
      "~/org/")))

(provide 'cb-org-directory)

;;; cb-org-directory.el ends here
