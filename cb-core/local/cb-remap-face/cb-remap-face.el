;;; cb-remap-face.el --- Compat with spacemacs' face remapping.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((dash "2.12.1"))

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

;; Implements a procedure

;;; Code:

(require 'dash)

(defvar cb-remap-face--face-remapping-alist face-remapping-alist
  "Hacky work-around to prevent spacemacs from killing face remaps.")

;;;###autoload
(defun cb-remap-face (from to)
  "Add a face remapping from FROM to TO.
Work around spacemacs' aggressive manipulation of `face-remapping-alist'."
  (add-to-list 'cb-remap-face--face-remapping-alist (cons from to))
  (add-to-list 'face-remapping-alist (cons from to)))

(cb-remap-face 'flymake-errline 'flycheck-error)
(cb-remap-face 'flymake-warnling 'flycheck-warning)

(defun cb-remap-face-restore (&rest _)
  "Restore remapped faces.  Useful as an after-advice."
  (setq face-remapping-alist (-concat face-remapping-alist cb-remap-face--face-remapping-alist)))

(provide 'cb-remap-face)

;;; cb-remap-face.el ends here
