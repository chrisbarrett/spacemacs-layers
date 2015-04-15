;;; packages.el --- cb-elfeed Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar cb-elfeed-packages '(elfeed)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-elfeed-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-elfeed/init-elfeed ()
  (use-package elfeed
    :commands elfeed
    :init
    (evil-leader/set-key "af" 'elfeed)
    :config
    (progn
      (evil-set-initial-state 'elfeed-search-mode 'emacs)
      (evil-set-initial-state 'elfeed-show-mode 'emacs)

      (evilify elfeed-search-mode elfeed-search-mode-map
               (kbd "q") 'quit-window)

      (evilify elfeed-show-mode elfeed-show-mode-map
               (kbd "q") 'elfeed-kill-buffer)

      (defconst cb-elfeed/update-timer
        (run-with-timer 1 (* 60 60) 'elfeed-update)))))
