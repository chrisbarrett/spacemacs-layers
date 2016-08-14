;;; packages.el --- cb-ledger Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-ledger-packages
  '(ledger-mode
    (cb-ledger-reports :location local)
    (cb-ledger-format :location local))
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun cb-ledger/user-config ()
  ;; Set this keybinding late so that Spacemacs does not clobber it.
  (spacemacs/set-leader-keys "o$" #'cb-ledger-goto-ledger-file))

(defun cb-ledger/post-init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\.ledger$" . ledger-mode)

    :bind
    (:map
     ledger-mode-map
     ("C-c C-c" . ledger-report)
     ("M-RET" . ledger-toggle-current-transaction)
     ("C-c C-." . cb-ledger-insert-timestamp))

    :evil-bind
    (:state
     normal
     :map
     ledger-report-mode-map
     ("q" . kill-buffer-and-window))

    :config
    (progn
      (setq ledger-master-file (f-join org-directory "accounts.ledger"))
      (with-eval-after-load 'cb-org-directory
        (setq ledger-master-file (f-join (cb-org-directory) "accounts.ledger")))

      (setq ledger-post-account-alignment-column 2)
      (setq ledger-post-use-completion-engine :ido)
      (setq ledger-fontify-xact-state-overrides nil)

      (setq ledger-reports
            `(("assets" "ledger -f %(ledger-file) bal assets")
              ("balance" "ledger -f %(ledger-file) bal")
              ("reg this week" "ledger -f %(ledger-file) reg checking -p 'this week' --invert")
              ("reg this month" "ledger -f %(ledger-file) reg checking -p 'this month' --invert")
              ("reg since payday" "ledger -f %(ledger-file) reg checking -b %(last-payday) --invert")
              ("reg previous pay period" "ledger -f %(ledger-file) reg checking -p %(prev-pay-period) --invert")))

      ;; Faces and font-locking

      (defface ledger-date
        `((t :inherit org-date :underline nil :foreground ,cb-vars-solarized-hl-cyan))
        "Face for dates at start of transactions."
        :group 'ledger-faces)

      (defface ledger-periodic-header
        `((t :foreground ,cb-vars-solarized-hl-violet :bold t))
        "Face for the header for periodic transactions."
        :group 'ledger-faces)

      (defface ledger-year-line
        `((t :foreground ,cb-vars-solarized-hl-violet))
        "Face for year declarations."
        :group 'ledger-faces)

      (defface ledger-report-negative-amount
        `((t (:foreground ,cb-vars-solarized-hl-red)))
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

      (custom-set-faces
       '(ledger-occur-xact-face
         ((((background dark))  :background "#073642")
          (((background light)) :background "#eee8d5")))
       `(ledger-font-pending-face
         ((t (:foreground ,cb-vars-solarized-hl-orange))))
       `(ledger-font-payee-cleared-face
         ((t (:foreground ,cb-vars-solarized-hl-green))))
       `(ledger-font-payee-uncleared-face
         ((t (:foreground ,cb-vars-solarized-hl-orange))))
       `(ledger-font-posting-account-face
         ((t (:foreground ,cb-vars-solarized-hl-blue)))))

      (cb-remap-face 'ledger-font-comment-face 'font-lock-comment-face)

      ;; Fix font lock issue in ledger reports
      (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer)

      (defun cb-ledger/report-from-report-buffer ()
        (interactive)
        (let ((buf (--first (with-current-buffer it
                              (derived-mode-p 'ledger-mode))
                            (buffer-list))))
          (pop-to-buffer buf)
          (call-interactively #'ledger-report)))

      (define-key ledger-report-mode-map (kbd "C-c C-c") #'cb-ledger/report-from-report-buffer)

      ;; Hide command name from reports.

      (with-eval-after-load 'ledger-report
        (defun ledger-do-report (cmd)
          (goto-char (point-min))
          (insert (format "Report: %s\n" ledger-report-name)
                  (make-string (- (window-width) 1) ?=)
                  "\n\n")
          (let ((data-pos (point))
                (register-report (string-match " reg\\(ister\\)? " cmd))
                files-in-report)
            (shell-command
             ;; --subtotal does not produce identifiable transactions, so don't
             ;; prepend location information for them
             cmd
             t nil)
            (when register-report
              (goto-char data-pos)
              (while (re-search-forward "^\\(/[^:]+\\)?:\\([0-9]+\\)?:" nil t)
                (let ((file (match-string 1))
                      (line (string-to-number (match-string 2))))
                  (delete-region (match-beginning 0) (match-end 0))
                  (when file
                    (set-text-properties (line-beginning-position) (line-end-position)
                                         (list 'ledger-source (cons file (save-window-excursion
                                                                           (save-excursion
                                                                             (find-file file)
                                                                             (widen)
                                                                             (ledger-navigate-to-line line)
                                                                             (point-marker))))))
                    (add-text-properties (line-beginning-position) (line-end-position)
                                         (list 'font-lock-face 'ledger-font-report-clickable-face))
                    (end-of-line)))))
            (goto-char data-pos)))))))

(defun cb-ledger/init-cb-ledger-reports ()
  (use-package cb-ledger-reports
    :after ledger-mode
    :init
    (add-hook 'ledger-mode-hook #'cb-ledger-reports-init)
    :config
    (setq cb-ledger-reports-income-payee-name "Income:Movio")))

(defun cb-ledger/init-cb-ledger-format ()
  (use-package cb-ledger-format
    :after ledger-mode
    :bind (:map ledger-mode-map ("M-q" . cb-ledger-format-buffer))))
