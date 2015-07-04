;;; extensions.el --- cb-calc Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-calc-pre-extensions
  '(calc)
  "List of all extensions to load before the packages.")

(defconst cb-calc-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-calc/init-calc ()
  (use-package calc
    :defer t
    :bind (("C-x c" . quick-calc))
    :config
    (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
                                  (MiB "1024 * KiB" "Mega Byte")
                                  (KiB "1024 * B" "Kilo Byte")
                                  (B nil "Byte")
                                  (Gib "1024 * Mib" "Giga Bit")
                                  (Mib "1024 * Kib" "Mega Bit")
                                  (Kib "1024 * b" "Kilo Bit")
                                  (b "B / 8" "Bit")))))
