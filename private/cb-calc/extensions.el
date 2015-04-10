;;; extensions.el --- cb-calc Layer extensions File for Spacemacs
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

(defvar cb-calc-pre-extensions
  '(calc)
  "List of all extensions to load before the packages.")

(defvar cb-calc-post-extensions
  '()
  "List of all extensions to load after the packages.")

(defun cb-calc/init-calc ()
  (use-package calc
    :defer t
    :config
    (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
                                  (MiB "1024 * KiB" "Mega Byte")
                                  (KiB "1024 * B" "Kilo Byte")
                                  (B nil "Byte")
                                  (Gib "1024 * Mib" "Giga Bit")
                                  (Mib "1024 * Kib" "Mega Bit")
                                  (Kib "1024 * b" "Kilo Bit")
                                  (b "B / 8" "Bit")))))
