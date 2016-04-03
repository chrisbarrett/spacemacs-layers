;;; packages.el --- cb-coffeescript Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-coffeescript-packages
  '(coffee-mode))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-coffeescript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t))

;;; packages.el ends here
