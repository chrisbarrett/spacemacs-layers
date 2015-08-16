;;; extensions.el --- cb-sml Layer extensions File for Spacemacs
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

(defconst cb-sml-pre-extensions '())

(defconst cb-sml-post-extensions '(smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-sml/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn

      (let ((ops (-flatten-n 1 (list
                                (smart-ops "," ";" :pad-before nil)
                                (smart-ops "*" "^" "@")
                                (smart-ops-default-ops)))))

        (define-smart-ops-for-mode 'sml-mode ops)
        (define-smart-ops-for-mode 'lazy-sml-mode ops)
        (define-smart-ops-for-mode 'inferior-sml-mode ops))

      (add-hook 'inferior-sml-mode-hook 'smart-ops-mode))))
