;;; packages.el --- cb-ocaml Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst cb-ocaml-packages
  '(merlin
    tuareg
    utop
    flycheck-ocaml
    aggressive-indent
    (merlin-eldoc :location local)
    (smart-ops :location local)
    (ocaml-autoinsert :location local)))

(eval-when-compile
  (require 'dash)
  (require 'f)
  (require 's)
  (require 'use-package nil t))

(defun cb-ocaml/post-init-tuareg ()

  (setq tuareg-function-indent 2)
  (setq tuareg-match-indent 0)

  (with-eval-after-load 'tuareg
    (core/remap-face 'tuareg-font-lock-governing-face 'font-lock-keyword-face)
    (core/remap-face 'tuareg-font-lock-operator-face 'default)

    (with-eval-after-load 'projectile
      (define-key tuareg-mode-map (kbd "C-c C-c") 'projectile-compile-project))

    (define-key tuareg-mode-map (kbd "M-RET") 'cb-ocaml/m-ret))

  (font-lock-add-keywords
   'tuareg-mode
   `(,(cb-core-font-lock-replace-match (rx space (group "->") space) 1 "→")
     ("->" . font-lock-keyword-face)
     (,(rx (or bol space) "|" (or eol space)) . font-lock-keyword-face)
     )))

(defun cb-ocaml/post-init-utop ()

  (defun cb-ocaml/set-utop-command ()
    (-when-let* ((ocamlinit ".ocamlinit")
                 (dir (f-traverse-upwards
                       (lambda (dir)
                         (f-files dir (lambda (file)
                                        (equal (f-filename file) ocamlinit))))))
                 (file (f-abbrev (f-join dir ocamlinit)))
                 )
      (setq-local utop-command (format "utop -emacs -init %s" file))))

  (add-hook 'merlin-mode-hook 'cb-ocaml/set-utop-command)

  (with-eval-after-load 'merlin
    (define-key merlin-mode-map (kbd "C-c C-.") 'merlin-locate)
    (define-key merlin-mode-map (kbd "C-c C-l") nil))

  (with-eval-after-load 'utop
    (define-key utop-minor-mode-map (kbd "C-c C-l") 'utop-eval-buffer)
    (define-key utop-minor-mode-map (kbd "C-c C-z") 'utop)))

(defun cb-ocaml/post-init-merlin ()
  (with-eval-after-load 'merlin
    (core/remap-face 'merlin-type-face 'cb-faces-bg-hl-ok)
    (evil-define-key 'normal merlin-mode-map (kbd "M-.") 'merlin-locate)
    (define-key merlin-mode-map (kbd "M-.") 'merlin-locate))
  (with-eval-after-load 'tuareg
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))))

(defun cb-ocaml/init-flycheck-ocaml ()
  (with-eval-after-load 'merlin
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup)))

(defun cb-ocaml/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'tuareg-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'utop-mode)))

(defun cb-ocaml/init-merlin-eldoc ()
  (with-eval-after-load 'merlin
    (require 'merlin-eldoc)))

(defun cb-ocaml/post-init-smart-ops ()

  (defun cb-ocaml/at-positional-or-optional-argument? ()
    (save-excursion
      (skip-chars-backward "_:[:alnum:]")
      (let ((ch (char-to-string (char-before))))
        (-contains? '("?" "~") ch))))

  (let ((common-ops
         (-flatten-n
          1
          (list
           (smart-ops "@" "^")
           (smart-ops "," ";" :pad-before nil)
           (smart-ops "." :pad-before nil :pad-after nil)
           (smart-ops "~" "?" :pad-after nil)
           (smart-ops ":"
                      :pad-unless
                      (lambda (pos)
                        (cb-ocaml/at-positional-or-optional-argument?)))

           (smart-ops-default-ops)))))

    (define-smart-ops-for-mode 'tuareg-mode
      common-ops
      (smart-ops ";;"
                 :pad-before nil
                 :pad-after nil
                 :action
                 (lambda (&rest _)
                   (comment-indent-new-line))))

    (define-smart-ops-for-mode 'utop-mode
      common-ops
      (smart-ops ";;"
                 :pad-before nil
                 :pad-after nil
                 :action
                 (lambda (&rest _)
                   (call-interactively 'utop-eval-input))))))

(defun cb-ocaml/init-ocaml-autoinsert ()
  (use-package ocaml-autoinsert
    :functions (ocaml-autoinsert-init)
    :config (ocaml-autoinsert-init)))

;;; packages.el ends here
