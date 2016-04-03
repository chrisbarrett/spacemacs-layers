;;; ocaml-autoinsert.el --- Autoinsert configuration for OCaml.  -*- lexical-binding: t; -*-

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

(defconst ocaml-autoinsert-form
  '(("\\.ml\\'" . "OCaml Src File")
    nil
    "open Core.Std" "\n\n"
    _
    "\n"))

;;;###autoload
(defun ocaml-autoinsert-init ()
  (with-eval-after-load 'autoinsert
    (with-no-warnings
      (add-to-list 'auto-insert-alist ocaml-autoinsert-form))))

(provide 'ocaml-autoinsert)

;;; ocaml-autoinsert.el ends here
