;;; extensions.el --- cb-agda Layer extensions File for Spacemacs
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

(defvar cb-agda-pre-extensions
  '(
    ;; pre extension cb-agdas go here
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-agda-post-extensions
  '(
    ;; post extension cb-agdas go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-agda/init-<extension-cb-agda>
;;
;; (defun cb-agda/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-agda/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'agda2-mode
      :add '("$"))))
