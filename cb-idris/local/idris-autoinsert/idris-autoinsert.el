;;; idris-autoinsert.el --- Autoinsert configuration for Idris. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0"))

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

(require 's)

(defconst idris-autoinsert-form
  '((idris-mode . "Idris Src File")
    nil
    "module " (idris-autoinsert--module-name) "\n"
    "\n"
    "%default total\n"
    "\n"
    _
    "\n"))

(defun idris-autoinsert--module-name ()
  (s-capitalize (file-name-base)))

(defun idris-autoinsert-init ()
  (with-eval-after-load 'autoinsert
    (with-no-warnings
      (add-to-list 'auto-insert-alist idris-autoinsert-form))))

(provide 'idris-autoinsert)

;;; idris-autoinsert.el ends here
