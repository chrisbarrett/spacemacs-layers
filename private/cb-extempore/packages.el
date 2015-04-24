;;; packages.el --- Packages required for extempore.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar cb-extempore-packages
  '(osc)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-extempore-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-extempore/init-osc ()
  (use-package osc
    :defer t))
