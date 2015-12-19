;;; packages.el --- cb-ledger Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-ledger-packages
  '(ledger-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun cb-ledger/user-config ()
  ;; Set this keybinding late so that Spacemacs does not clobber it.
  (evil-leader/set-key "o$" 'ledger/goto-ledger-file))

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

(defun cb-ledger/post-init-ledger-mode ()
  (add-to-list 'auto-mode-alist (cons "\\.ledger$" 'ledger-mode))

  (setq ledger-master-file (f-join org-directory "accounts.ledger"))
  (setq ledger-post-account-alignment-column 2)
  (setq ledger-post-use-completion-engine :ido)
  (setq ledger-fontify-xact-state-overrides nil)

  (defconst cb-ledger/income-account-name "Income:Movio")

  (defun cb-ledger/last-n-pay-dates (n)
    (let ((output (shell-command-to-string
                   (format "ledger -f '%s' reg '%s' -y '%%Y-%%m-%%d' | tail -n %s | awk '{print $1}'"
                           ledger-master-file
                           cb-ledger/income-account-name
                           n))))
      (s-split "\n" output t)))

  (defun cb-ledger/last-payday ()
    (car (cb-ledger/last-n-pay-dates 1)))

  (defun cb-ledger/previous-pay-period ()
    (-let [(prev cur) (cb-ledger/last-n-pay-dates 2)]
      (format "from %s to %s" prev (cb-ledger/day-before cur))))

  (defun cb-ledger/day-before (ledger-date-str)
    (-let* (((y m d) (-map 'string-to-number (s-split "[/-]" ledger-date-str)))
            (date (encode-time 0 0 0 d m y))
            (updated (time-subtract date (days-to-time 1))))
      (format-time-string "%Y-%m-%d" updated)))

  (setq ledger-report-format-specifiers
        '(("ledger-file" . ledger-report-ledger-file-format-specifier)
          ("last-payday" . cb-ledger/last-payday)
          ("prev-pay-period" . cb-ledger/previous-pay-period)
          ("payee" . ledger-report-payee-format-specifier)
          ("account" . ledger-report-account-format-specifier)
          ("tagname" . ledger-report-tagname-format-specifier)
          ("tagvalue" . ledger-report-tagvalue-format-specifier)))

  (setq ledger-reports
        '(("assets" "ledger -f %(ledger-file) bal assets")
          ("balance" "ledger -f %(ledger-file) bal")
          ("register" "ledger -f %(ledger-file) reg")
          ("payee" "ledger -f %(ledger-file) reg @%(payee)")
          ("account" "ledger -f %(ledger-file) reg %(account)")
          ("net worth" "ledger -f %(ledger-file) bal ^assets ^liabilities")
          ("cash flow" "ledger -f %(ledger-file) bal ^income ^expenses")
          ("this week" "ledger -f %(ledger-file) -p 'this week' -r reg 'checking' --invert")
          ("this month" "ledger -f %(ledger-file) -p 'this month' -r reg 'checking' --invert")
          ("reg since payday" "ledger -f %(ledger-file) -b %(last-payday) -r reg 'checking' --invert")
          ("bal since payday" "ledger -f %(ledger-file) -b %(last-payday) -r bal 'checking' --invert")

          ("reg last pay period" "ledger -f %(ledger-file) -p %(prev-pay-period) -r reg 'checking' --invert")
          ("bal last pay period" "ledger -f %(ledger-file) -p %(prev-pay-period) -r bal 'checking' --invert")))

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
  (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer))
