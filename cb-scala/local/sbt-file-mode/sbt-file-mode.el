;;; sbt-file-mode.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((scala-mode "20151226.1048"))

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

(autoload 'scala-mode "scala-mode")

;;;###autoload
(define-derived-mode sbt-file-mode scala-mode "SBT"
  "Major mode for editing SBT files.

When started, runs `sbt-file-mode-hook'.

\\{sbt-file-mode-map}")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-file-mode))

(provide 'sbt-file-mode)

;;; sbt-file-mode.el ends here
