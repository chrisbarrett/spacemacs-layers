;;; cb-go-flycheck-metalinter-unique-errors.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris@movio.co>
;; Package-Requires: ((s "1.10.0") (flycheck "20160924.1038") (dash "2.12.1"))

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
(require 'flycheck)
(require 's)

(defun cb-go-flycheck-metalinter-unique-errors--message-sans-checker (err)
  (-let [(_all msg) (s-match (rx (group (+? any))
                                 (+ space) "(" (+? nonl) ")" eos)
                             (flycheck-error-message err))]
    msg))

(defun cb-go-flycheck-metalinter-unique-errors--remove-if-present (&optional errs err)
  (cond
   ((null errs)
    (list err))
   (errs
    (let ((existing (-map #'cb-go-flycheck-metalinter-unique-errors--message-sans-checker errs)))
      (cond ((-contains? existing (cb-go-flycheck-metalinter-unique-errors--message-sans-checker err))
             errs)
            (t
             (cons err errs)))))
   (t
    nil)))

(defun cb-go-flycheck-metalinter-unique-errors--filter-errors (errors)
  (let ((reduced (-reduce-from #'cb-go-flycheck-metalinter-unique-errors--remove-if-present nil errors)))
    (->
     (--map (let ((msg (cb-go-flycheck-metalinter-unique-errors--message-sans-checker it)))
              (setf (flycheck-error-message it) msg)
              it)
            reduced)
     flycheck-dedent-error-messages
     flycheck-sanitize-errors)))

(defun cb-go-flycheck-metalinter-unique-errors-init ()
  (put 'gometalinter 'flycheck-error-filter #'cb-go-flycheck-metalinter-unique-errors--filter-errors))

(provide 'cb-go-flycheck-metalinter-unique-errors)

;;; cb-go-flycheck-metalinter-unique-errors.el ends here
