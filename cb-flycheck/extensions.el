;;; extensions.el --- cb-flycheck Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-flycheck-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defconst cb-flycheck-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))
