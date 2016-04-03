;;; cb-faces.el --- Custom faces used throughout config.  -*- lexical-binding: t; -*-

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

(defface cb-faces-bg-hl-ok
  '((((background dark))  :background "#01304b")
    (((background light)) :background "#e9f2c5"))
  "Face for highlighting regions that represent an 'OK' state."
  :group 'cb-faces)

(defface cb-faces-bg-hl-template
  '((((background dark))  :background "#3f4d91")
    (((background light)) :background "thistle"))
  "Face for active template fields."
  :group 'cb-faces)

(defface cb-faces-bg-hl-red
  '((((background dark))  :background "#51202b")
    (((background light)) :background "#fee8e5"))
  "Face for highlighting regions that represent an error state."
  :group 'cb-faces)

(defface cb-faces-bg-flash
  '((((class color) (background light))
     :background "darkseagreen2")
    (((class color) (background dark))
     :background "royalblue4"))
  "Face for flashing with a green background."
  :group 'cb-faces)

(defface cb-faces-bg-flash-red
  '((t (:background "rosybrown1")))
  "Face for flashing with a red background."
  :group 'cb-faces)

(provide 'cb-faces)

;;; cb-faces.el ends here
