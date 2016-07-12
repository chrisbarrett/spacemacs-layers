;;; cb-web-modes.el --- Specialised web mode derivatives. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((web-mode "20160702.718"))

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
;; Specialised web mode derivatives.

;;; Code:

(require 'web-mode)

;;;###autoload
(define-derived-mode cb-web-js-mode web-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode cb-web-json-mode web-mode "JSON"
  "Derived mode for editing JSON files.")

;;;###autoload
(define-derived-mode cb-web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files.")

(provide 'cb-web-modes)

;;; cb-web-modes.el ends here
