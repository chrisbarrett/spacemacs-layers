;;; packages.el --- cb-crontab Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-crontab-packages
  '(crontab-mode))

(defconst cb-crontab-excluded-packages '())

(eval-when-compile
  (require 'use-package nil t))

(defun cb-crontab/init-crontab-mode ()
  (use-package crontab-mode
    :defer t
    :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
           ("cron\\(tab\\)?\\."    . crontab-mode)
           ("/cron.d/" . crontab-mode))))
