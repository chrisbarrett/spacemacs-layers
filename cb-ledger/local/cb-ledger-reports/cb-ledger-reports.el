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

(require 'dash)
(require 'ledger-mode)
(require 's)

(autoload 'org-fill-paragraph "org")

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

(defun cb-ledger-reports--node-to-shell-cmd (node)
  (-let [(specifier . args) node]
    (let ((header-args
           (lambda (title)
             (format "\n\n%s\n%s\n" title (s-repeat (length title) "=")))))

      (apply (pcase specifier
               (`header-args header-args)

               (`header-1
                (lambda (title)
                  (concat "echo " (shell-quote-argument (s-trim-left (funcall header-args title))))))

               (`header
                (lambda (title)
                  (concat "echo " (shell-quote-argument (funcall header-args title)))))

               (`paragraph
                (lambda (s)
                  (let ((filled
                         (with-temp-buffer
                           (org-mode)
                           (insert s)
                           (org-fill-paragraph)
                           (while (zerop (forward-line))
                             (org-fill-paragraph))
                           (goto-char (point-max))
                           (newline)
                           (newline)
                           (buffer-string))))
                    (format "echo %s" (shell-quote-argument filled)))))

               (`separator
                (lambda ()
                  (format "echo %s" (shell-quote-argument (concat "\n\n" (s-repeat 80 "=") "\n")))))

               (s
                (error "Unknown markup specifier: %s" s)))

             args))))

(defun cb-ledger-reports--report-shell-cmd-from-spec (spec)
  (substring-no-properties
   (s-join " && "
           (-tree-map-nodes
            (lambda (node)
              (and (listp node)
                   (symbolp (car node))))
            #'cb-ledger-reports--node-to-shell-cmd
            spec))))

(defconst cb-ledger-reports-weekly-review
  '((paragraph "Skim over balances to make sure they look right.")

    (header-1 "Assets")
    "ledger -f %(ledger-file) bal Assets --depth 2"

    (header "Expenses Last 7 Days")
    "ledger -f %(ledger-file) bal expenses --sort total -p 'last 7 days' --invert"
    (header "Week-on-week change Last 7 Days")
    (paragraph "How much money went in/out of my accounts?")
    "ledger -f %(ledger-file) bal assets -p 'last 7 days'"

    (separator)
    (paragraph "Skim the totals below, which are tallied against my budget.
- Am I meeting my budget?
- If not, what are the areas that need improvement?

These balances show the remaining available balance for each category.")

    (header-1 "Budget Last 7 Days")
    "ledger -f %(ledger-file) bal expenses --sort total -p 'last 7 days' --invert --budget"
    (header "Budget Last 30 Days")
    "ledger -f %(ledger-file) bal expenses --sort total -p 'last 30 days' --invert --budget"
    (header "Budget Since Payday")
    "ledger -f %(ledger-file) bal expenses --sort total -b %(last-payday) --invert --budget"
    (header "Budget Last Pay Period")
    "ledger -f %(ledger-file) bal expenses --sort total -p %(prev-pay-period) --invert --budget"

    (separator)
    (paragraph "The payees below are organised by total spending against the budget.
- Any places where I tend to spend excessively?
- Any opportunity for savings?
- Any habits I could change to spend more wisely?")

    (header-1 "Budget Last 7 Days, By Payee")
    "ledger -f %(ledger-file) reg expenses --by-payee --sort total -p 'last 7 days' --invert --budget"
    (header "Budget Last 30 Days, By Payee")
    "ledger -f %(ledger-file) reg expenses --by-payee --sort total -p 'last 30 days' --invert --budget"

    (separator)
    (paragraph "Read through the payees below from my checking account. Any spending patterns here that could be budgeted?")
    (header-1 "Unbudgeted Spending, Last 7 Days")
    "ledger -f %(ledger-file) bal expenses --sort total -p 'last 7 days' --invert --unbudgeted"
    (header "Register")
    "ledger -f %(ledger-file) reg checking --by-payee --sort total -p 'last 7 days' --invert"))

(defconst cb-ledger-reports-expenses
  '((header-1 "Expenses For Week")
    "ledger -f %(ledger-file) bal expenses -p 'this week' --invert"
    (header "Expenses For Month")
    "ledger -f %(ledger-file) bal expenses -p 'this month' --invert"
    (header "Expenses Since Payday")
    "ledger -f %(ledger-file) bal expenses -b %(last-payday) --invert"
    (header "Expenses Previous Pay Period")
    "ledger -f %(ledger-file) bal expenses -p %(prev-pay-period) --invert"))

(defun cb-ledger-reports--alist-insert (k v alist)
  (if (rassoc k alist)
      (setf (cdr (rassoc k alist)) v)
    (nconc alist `(,(cons k v)))))

(defun cb-ledger-reports--list-insert (k v ls)
  (cl-delete-if (lambda (it) (equal k (car it))) ls)
  (nconc ls `((,k ,v))))

(defun cb-ledger-reports-init ()
  (cb-ledger-reports--alist-insert "last-payday" #'cb-ledger-reports-last-payday ledger-report-format-specifiers)
  (cb-ledger-reports--alist-insert "prev-pay-period" #'cb-ledger-reports-previous-pay-period ledger-report-format-specifiers)
  (cb-ledger-reports--list-insert "weekly review" (cb-ledger-reports--report-shell-cmd-from-spec cb-ledger-reports-weekly-review) ledger-reports)
  (cb-ledger-reports--list-insert "expenses" (cb-ledger-reports--report-shell-cmd-from-spec cb-ledger-reports-expenses) ledger-reports))

(provide 'cb-ledger-reports)

;;; cb-ledger-reports.el ends here
