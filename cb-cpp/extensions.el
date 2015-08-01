;;; extensions.el --- cb-cpp Layer extensions File for Spacemacs
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

(defconst cb-cpp-pre-extensions '(cc-mode))

(defconst cb-cpp-post-extensions '(super-smart-ops))

(eval-when-compile
  (require 'use-package nil t))

(defun cb-cpp/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    ;; HACK: The c-style way of setting up derived maps dynamically deeply
    ;; sucks. Just clobber that shit.
    (defconst c++-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "#"        'c-electric-pound)
        (define-key map "\C-c\C-e" 'c-macro-expand)
        map))
    :config
    (progn
      (evil-define-key 'insert c++-mode-map
        (kbd "<backspace>") 'sp/generic-prog-backspace
        (kbd "SPC") 'sp/generic-prog-space)

      (define-key c++-mode-map (kbd "M-RET") 'cb-cpp/M-RET)
      (define-key c++-mode-map (kbd "C-<return>") 'cb-cpp/C-RET)

      ;; Aggressive indent is a little too aggressive for C++.

      (with-eval-after-load 'aggressive-indent
        (add-to-list 'aggressive-indent-excluded-modes 'c++-mode))

      (defun cb-cpp/set-local-hooks ()
        (add-hook 'before-save-hook 'core/indent-buffer nil t))

      (add-hook 'c++-mode-hook 'cb-cpp/set-local-hooks)

      ;; Font-locking

      (font-lock-add-keywords 'c++-mode `((";" 0 font-lock-comment-face t)))
      (font-lock-add-keywords 'c++-mode `(("\\_<constexpr\\_>" 0 font-lock-keyword-face t)))

      ;; Flyspell

      (defun cb-cpp/flyspell-verify ()
        "Do not spellcheck imports."
        (and (flyspell-generic-progmode-verify)
             (not (s-matches? (rx bol (* space) "#") (current-line)))))

      (defun cb-cpp/configure-flyspell ()
        (setq-local flyspell-generic-check-word-predicate 'cb-cpp/flyspell-verify))

      (add-hook 'c++-mode-hook 'cb-cpp/configure-flyspell)

      )))

(defun cb-cpp/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (super-smart-ops-configure-for-mode 'c++-mode
      :custom '((">" . cb-cpp/>)
                ("&" . cb-cpp/&)
                ("*" . cb-cpp/*)
                (":" . cb-cpp/:)
                ))))
