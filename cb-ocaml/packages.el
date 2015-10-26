;;; packages.el --- cb-ocaml Layer packages File for Spacemacs
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

(defconst cb-ocaml-packages
  '(merlin
    utop
    flycheck-ocaml
    aggressive-indent
    (smart-ops :location local)))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-ocaml/post-init-utop ()
  (with-eval-after-load 'utop
    (define-key utop-minor-mode-map (kbd "C-c C-l") 'utop-eval-buffer)
    (define-key utop-minor-mode-map (kbd "C-c C-z") 'utop)))

(defun cb-ocaml/post-init-merlin ()
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))

(defun cb-ocaml/init-flycheck-ocaml ()
  (with-eval-after-load 'merlin
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup)))

(defun cb-ocaml/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'tuareg-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'utop-mode)))

(defun cb-ocaml/post-init-smart-ops ()
  (let ((ops
         (-flatten-n 1
                     (list
                      (smart-ops "," ";" :pad-before nil)
                      (smart-ops "." :pad-before nil :pad-after nil)
                      (smart-ops ";;" :pad-before nil :pad-after nil
                                 :action
                                 (lambda (&rest _)
                                   (comment-indent-new-line)))
                      (smart-ops-default-ops)))))
    (define-smart-ops-for-mode 'tuareg-mode ops)
    (define-smart-ops-for-mode 'utop-mode ops)))
