;;; cb-ledger-reports.el --- Utilities for generated ledger reports. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((s "1.10.0") (ledger-mode "20160111.1834") (dash "2.12.1"))
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

(require 's)
(require 'dash)

(defgroup cb-ledger-reports nil
  "Utilities for generating ledger reports."
  :group 'finance
  :prefix "cb-ledger-reports-")

(defcustom cb-ledger-reports-income-payee-name nil
  "The account name for incoming funds."
  :group 'cb-ledger-reports
  :type 'string)

(defun cb-ledger-reports-last-n-pay-dates (n)
  (let ((output (shell-command-to-string
                 (format "ledger -f '%s' reg '%s' -y '%%Y-%%m-%%d' | tail -n %s | awk '{print $1}'"
                         ledger-master-file
                         cb-ledger-reports-income-payee-name
                         n))))
    (s-split "\n" output t)))

(defun cb-ledger-reports-last-payday ()
  (car (cb-ledger-reports-last-n-pay-dates 1)))

(defun cb-ledger-reports-previous-pay-period ()
  (-let [(prev cur) (cb-ledger-reports-last-n-pay-dates 2)]
    (format "from %s to %s" prev (cb-ledger-reports--day-before cur))))

(defun cb-ledger-reports--day-before (ledger-date-str)
  (-let* (((y m d) (-map 'string-to-number (s-split "[/-]" ledger-date-str)))
          (date (encode-time 0 0 0 d m y))
          (updated (time-subtract date (days-to-time 1))))
    (format-time-string "%Y-%m-%d" updated)))

(provide 'cb-ledger-reports)

;;; cb-ledger-reports.el ends here
