;;; cb-org-work.el --- Calculate the location of the work file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1"))

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

(require 'dash)
(require 'f)
(require 's)
(require 'cb-org-directory)

;;;###autoload
(defun cb-org-work-file ()
  (-if-let (work-files (f-files (cb-org-directory)
                                (lambda (file)
                                  (and (s-starts-with? "work_" (f-filename file))
                                       (f-ext? file "org")))))
      (car work-files)
    (f-join (cb-org-directory) "work.org")))

(provide 'cb-org-work)

;;; cb-org-work.el ends here
