;;; extensions.el --- cb-autoinsert Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-autoinsert-packages
  '(autoinsert))

(defun cb-autoinsert/init-autoinsert ()
  (use-package autoinsert
    :init (setq auto-insert-alist nil)
    :config
    (progn
      (add-to-list 'auto-insert-alist
                   '((html-mode . "HTML file")
                     nil
                     "<!DOCTYPE html>"          "\n"
                     "<html>"                   "\n"
                     "  <head>"                 "\n"
                     "    <title>" _ "</title>" "\n"
                     "  </head>"                "\n"
                     "  <body>"                 "\n"
                     "  </body>"                "\n"
                     "</html>"                  "\n"))

      (setq auto-insert-query nil)
      (auto-insert-mode +1))))

;;; packages.el ends here
