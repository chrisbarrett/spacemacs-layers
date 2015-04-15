;;; packages.el --- cb-flycheck Layer packages File for Spacemacs
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

(defvar cb-flycheck-packages
  '(
    flycheck
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-flycheck-excluded-packages '(flycheck-pos-tip)
  "List of packages to exclude.")

;; For each package, define a function cb-flycheck/init-<package-cb-flycheck>
;;
;; (defun cb-flycheck/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-flycheck/init-flycheck ()
  (use-package flycheck
    :config
    (progn
      (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
      (setq flycheck-check-syntax-automatically '(mode-enabled idle-change save))
      (global-flycheck-mode +1))))
