;;; packages.el --- cb-crontab Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-crontab-packages
  '(crontab-mode))

(defun cb-crontab/init-crontab-mode ()
  (use-package crontab-mode
    :defer t
    :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
           ("cron\\(tab\\)?\\."    . crontab-mode)
           ("/cron.d/" . crontab-mode))))
