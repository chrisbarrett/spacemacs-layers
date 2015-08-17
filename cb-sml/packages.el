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
    :init
    (progn
      (define-derived-mode lazy-sml-mode sml-mode "SML[lazy]"
        "Extend `sml-mode' with support for laziness language extensions.")

      (add-to-list 'auto-mode-alist '("\\.lml\\'" . lazy-sml-mode))

      (font-lock-add-keywords
       'lazy-sml-mode
       `((,(rx bol (* space) "fun" (+ space) (group "lazy") (+ space) (group (+ word)) eow)
          (1 font-lock-type-def-face)
          (2 font-lock-function-name-face)))))

    :config
    (progn
      (setq sml-indent-level 2)

      ;; Enable support for laziness language extensions. Requires SMLNJ.
      (setq sml-default-arg "-Cparser.lazy-keyword=true")

      (add-to-list 'aggressive-indent-excluded-modes 'sml-mode)
      (define-key inferior-sml-mode-map (kbd "M-RET") 'cb-sml/inf-sml-m-ret)
      (define-key sml-mode-map (kbd "M-RET") 'cb-sml/m-ret))))
