;;; packages.el --- cb-sml Layer packages File for Spacemacs
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

(defconst cb-sml-packages '(sml-mode))

(defconst cb-sml-excluded-packages '())

(eval-when-compile
  (require 'flycheck nil t)
  (require 'use-package nil t))

(defun cb-sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :commands (run-sml sml-mode)
    :config
    (progn
      (setq sml-indent-level 2)

      (add-to-list 'aggressive-indent-excluded-modes 'sml-mode)
      (define-key inferior-sml-mode-map (kbd "M-RET") 'cb-sml/inf-sml-m-ret)
      (define-key sml-mode-map (kbd "M-RET") 'cb-sml/m-ret)

      (defadvice sml-prog-proc-switch-to (after append-buffer activate)
        (goto-char (point-max))
        (when (thing-at-point-looking-at sml-prompt-regexp)
          (evil-insert-state))))))
