;;; merlin-eldoc.el --- Eldoc support for Merlin  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

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

(require 'merlin)
(require 's)

(defun merlin-eldoc/eldoc-function ()
  (merlin-sync-to-point)
  (when (merlin--type-enclosing-query)
    (-when-let (res (merlin-type-enclosing-go-up))
      (s-trim res))))

(defun merlin-eldoc/setup ()
  (eldoc-mode +1)
  (setq-local eldoc-documentation-function #'merlin-eldoc/eldoc-function))

(add-hook 'merlin-mode-hook #'merlin-eldoc/setup)

(provide 'merlin-eldoc)

;;; merlin-eldoc.el ends here
