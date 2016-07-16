;;; js-yasnippet.el --- Supporting functions for snippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0") (dash "2.12.1"))

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
(require 'dash)

(defun js-yasnippet--ctor-body (s)
  (when s
    (->> (s-split (rx (or "," ".")) s)
         (-map #'s-trim)
         (-remove #'s-blank?)
         (--map (format "this.%s = %s;" it it))
         (s-join "\n"))))

(provide 'js-yasnippet)

;;; js-yasnippet.el ends here
