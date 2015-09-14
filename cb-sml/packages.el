;;; packages.el --- cb-sml Layer packages File for Spacemacs
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

(defconst cb-sml-packages '(sml-mode))

(defconst cb-sml-excluded-packages '())

(eval-when-compile
  (require 'flycheck nil t)
  (require 'use-package nil t))

(defun cb-sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :commands (run-sml sml-mode)
    :config
    (progn
      (setq sml-indent-level 2)

      (add-to-list 'aggressive-indent-excluded-modes 'sml-mode)
      (define-key inferior-sml-mode-map (kbd "M-RET") 'cb-sml/inf-sml-m-ret)
      (define-key sml-mode-map (kbd "M-RET") 'cb-sml/m-ret)
      (define-key sml-mode-map (kbd "S-TAB") 'sml-back-to-outer-indent)
      (define-key sml-mode-map (kbd "<return>") 'cb-sml/ret)
      (define-key sml-mode-map (kbd "M-SPC") nil)

      (defun cb-sml/set-local-bindings ()
        (evil-local-set-key 'normal ",ct" 'flycheck-buffer))

      (add-hook 'sml-mode-hook 'cb-sml/set-local-bindings)

      (defadvice sml-prog-proc-switch-to (after append-buffer activate)
        (goto-char (point-max))
        (when (thing-at-point-looking-at sml-prompt-regexp)
          (evil-insert-state)))

      ;; Use font locking to display pretty arrows.

      (font-lock-add-keywords
       'sml-mode
       `(,(core/font-lock-replace-match (rx space (group "->") space) 1 "→")
         ,(core/font-lock-replace-match (rx space (group "=>") space) 1 "⇒")))

      ;; Advice to work around super-aggressive SML indentation.

      (defadvice evil-open-below (around sml-indent activate)
        (let ((col (current-indentation)))
          ad-do-it
          (when (and (derived-mode-p 'sml-mode) (s-blank? (s-trim (current-line))))
            (delete-horizontal-space)
            (indent-to col))))

      (defadvice evil-open-above (around sml-indent activate)
        (let ((col (current-indentation)))
          ad-do-it
          (when (and (derived-mode-p 'sml-mode) (s-blank? (s-trim (current-line))))
            (delete-horizontal-space)
            (indent-to col))))


      ;; Hack SMIE indentation so that structures are indented.

      (defun cb-sml/smie-rules (orig kind token)
        (pcase (cons kind token)
          (`(:after . "struct") 2)
          (_ (funcall orig kind token))))

      (add-hook 'sml-mode-hook
                (lambda ()
                  (add-function :around (symbol-function 'sml-smie-rules) #'cb-sml/smie-rules)))
      )))
