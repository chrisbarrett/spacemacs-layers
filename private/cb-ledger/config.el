;; Faces and font-locking

(defface ledger-date
  `((t :inherit org-date :underline nil :foreground ,solarized-hl-cyan))
  "Face for dates at start of transactions."
  :group 'ledger-faces)

(defface ledger-periodic-header
  `((t :foreground ,solarized-hl-violet :bold t))
  "Face for the header for periodic transactions."
  :group 'ledger-faces)

(defface ledger-year-line
  `((t :foreground ,solarized-hl-violet))
  "Face for year declarations."
  :group 'ledger-faces)

(defface ledger-report-negative-amount
  `((t (:foreground ,solarized-hl-red)))
  "Face for negative amounts in ledger reports."
  :group 'ledger-faces)

(font-lock-add-keywords
 'ledger-mode
 `((,(rx bol (+ (any digit "=" "/"))) . 'ledger-date)
   (,(rx bol "~" (* nonl)) . 'ledger-periodic-header)
   (,(rx bol "year" (+ space) (+ digit) (* space) eol) . 'ledger-year-line)))

(font-lock-add-keywords
 'ledger-report-mode
 `((,(rx "$" (* space) "-" (+ digit) (? "." (+ digit))) . 'ledger-report-negative-amount)
   (,(rx (+ digit) "-" (= 3 alpha) "-" (+ digit)) . 'ledger-date)))
