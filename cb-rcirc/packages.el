;;; packages.el --- cb-rcirc Layer packages File for Spacemacs
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

(defconst cb-rcirc-packages
  '((rcirc-reconnect :location local :disabled t)
    (rcirc-show-channels :location local)))

(defun cb-rcirc/init-rcirc-reconnect ()
  (use-package rcirc-reconnect
    :diminish rcirc-reconnect-mode
    :commands rcirc-reconnect-mode
    :init
    (add-hook 'rcirc-mode-hook 'rcirc-reconnect-mode)))

(defun cb-rcirc/init-rcirc-show-channels ()
  (use-package rcirc-show-channels
    :commands rcirc-show-channels
    :init
    (bind-key (kbd "<f7>") 'rcirc-show-channels)
    :config
    (progn
      (setq rcirc-show-channels-eyebrowse-window-config-number 1)
      (setq rcirc-time-format "%H:%M ")
      (setq rcirc-show-channels-priority
            '(("#haskell@irc.freenode.net" . 1))))))
