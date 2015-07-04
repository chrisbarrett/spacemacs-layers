;;; packages.el --- cb-coffeescript Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-coffeescript-packages
  '(coffee-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-coffeescript-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-coffeescript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t))
