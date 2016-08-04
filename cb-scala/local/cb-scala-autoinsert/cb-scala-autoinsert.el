;;; cb-scala-autoinsert.el --- Autoinsert configuration for Scala.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1") (projectile "0.13.0"))

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

(autoload 'projectile-project-p "projectile")

(defconst cb-scala-autoinsert-form
  '((scala-mode . "Scala Src File")
    nil
    (cb-scala-autoinsert--package-name)))

(defun cb-scala-autoinsert--package-name ()
  (-if-let* ((root (and (buffer-file-name) (projectile-project-p)))
             (pkg-id (->> (f-dirname (buffer-file-name))
                          (s-chop-prefix root)
                          f-split
                          nreverse
                          (--take-while (not (-contains? '("src" "app" "scala" "test" "tests") it)))
                          nreverse
                          (s-join "."))))
      (if (s-blank? pkg-id)
          ""
        (format "package %s\n\n" pkg-id))
    ""))

;;;###autoload
(defun cb-scala-autoinsert-init ()
  (with-eval-after-load 'autoinsert
    (with-no-warnings
      (add-to-list 'auto-insert-alist cb-scala-autoinsert-form))))

(provide 'cb-scala-autoinsert)

;;; cb-scala-autoinsert.el ends here
