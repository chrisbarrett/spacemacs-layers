;;; packages.el --- cb-ledger Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-ledger-packages
  '(ledger-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-ledger-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

(defun cb-ledger/init-ledger-mode ()
  (use-package ledger-mode
    :init
    (add-to-list 'auto-mode-alist (cons "\\.ledger$" 'ledger-mode))
    :config
    (progn
      (setq ledger-master-file (f-join org-directory "accounts.ledger"))
      (setq ledger-post-account-alignment-column 2)
      (setq ledger-post-use-completion-engine :ido)
      (setq ledger-fontify-xact-state-overrides nil)

      (defconst cb-ledger/pay-day-of-month 13)

      (defun cb-ledger/last-payday-at (date)
        (-let* (((s minute hour day month year) (decode-time date))
                (pay-day-of-month cb-ledger/pay-day-of-month)
                (before-pay-day? (< day pay-day-of-month))
                ((pay-month pay-year)
                 (cond
                  ((and (= 1 month) before-pay-day?)
                   (list 12 (1- year)))
                  (before-pay-day?
                   (list (1- month) year))
                  (t
                   (list month year)))))
          (encode-time s minute hour pay-day-of-month pay-month pay-year)))

      (setq ledger-reports
            (let* ((prev-payday-time (cb-ledger/last-payday-at (current-time)))
                   (time-before-last-payday (time-subtract prev-payday-time (days-to-time 1)))
                   (second-prev-payday-time (cb-ledger/last-payday-at time-before-last-payday))

                   (prev-payday (format-time-string "%Y-%m-%d" prev-payday-time))
                   (day-before-last-payday (format-time-string "%Y-%m-%d" time-before-last-payday))
                   (second-prev-payday (format-time-string "%Y-%m-%d" second-prev-payday-time))
                   (last-pay-period-expr (format "from %s to %s" second-prev-payday day-before-last-payday)))

              `(("assets" "ledger -f %(ledger-file) bal assets")
                ("balance" "ledger -f %(ledger-file) bal")
                ("register" "ledger -f %(ledger-file) reg")
                ("payee" "ledger -f %(ledger-file) reg @%(payee)")
                ("account" "ledger -f %(ledger-file) reg %(account)")
                ("net worth" "ledger -f %(ledger-file) bal ^assets ^liabilities")
                ("cash flow" "ledger -f %(ledger-file) bal ^income ^expenses")
                ("this week" "ledger -f %(ledger-file) -p 'this week' -r reg 'checking' --invert")
                ("this month" "ledger -f %(ledger-file) -p 'this month' -r reg 'checking' --invert")
                ("reg since payday"
                 ,(concat "ledger -f %(ledger-file) -b '" prev-payday
                          "' -r reg 'checking' --invert"))
                ("bal since payday"
                 ,(concat "ledger -f %(ledger-file) -b '" prev-payday
                          "' -r bal 'checking' --invert"))

                ("reg last pay period"
                 ,(concat "ledger -f %(ledger-file) -p '" last-pay-period-expr
                          "' -r reg 'checking' --invert"))
                ("bal last pay period"
                 ,(concat "ledger -f %(ledger-file) -p '" last-pay-period-expr
                          "' -r bal 'checking' --invert")))))

      (setq ledger-report-format-specifiers
            '(("ledger-file" . ledger-report-ledger-file-format-specifier)
              ("payee" . ledger-report-payee-format-specifier)
              ("account" . ledger-report-account-format-specifier)
              ("tagname" . ledger-report-tagname-format-specifier)
              ("tagvalue" . ledger-report-tagvalue-format-specifier)))

      (custom-set-faces
       '(ledger-occur-xact-face
         ((((background dark))  :background "#073642")
          (((background light)) :background "#eee8d5")))
       `(ledger-font-pending-face
         ((t (:foreground ,solarized-hl-orange))))
       `(ledger-font-payee-cleared-face
         ((t (:foreground ,solarized-hl-green))))
       `(ledger-font-payee-uncleared-face
         ((t (:foreground ,solarized-hl-orange))))
       `(ledger-font-posting-account-face
         ((t (:foreground ,solarized-hl-blue)))))

      (core/remap-face 'ledger-font-comment-face 'font-lock-comment-face)

      ;; Fix font lock issue in ledger reports
      (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer))))
