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
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defun cb-ocaml/post-init-tuareg ()
  (use-package tuareg
    :bind
    (:map tuareg-mode-map ("M-RET" . cb-ocaml/m-ret))
    :config
    (progn
      (setq tuareg-function-indent 2)
      (setq tuareg-match-indent 0)

      (cb-remap-face 'tuareg-font-lock-governing-face 'font-lock-keyword-face)
      (cb-remap-face 'tuareg-font-lock-operator-face 'default)

      (font-lock-add-keywords
       'tuareg-mode
       `(,(cb-font-lock-replace-match (rx space (group "->") space) 1 "→")
         ("->" . font-lock-keyword-face)
         (,(rx (or bol space) "|" (or eol space)) . font-lock-keyword-face)
         )))))

(defun cb-ocaml/post-init-utop ()
  (use-package utop
    :bind
    (:map utop-minor-mode-map
          ("C-c C-l" . utop-eval-buffer)
          ( "C-c C-z" . utop))
    :config
    (progn
      (defun cb-ocaml/set-utop-command ()
        (-when-let* ((ocamlinit ".ocamlinit")
                     (dir (f-traverse-upwards
                           (lambda (dir)
                             (f-files dir (lambda (file)
                                            (equal (f-filename file) ocamlinit))))))
                     (file (f-abbrev (f-join dir ocamlinit)))
                     )
          (setq-local utop-command (format "utop -emacs -init %s" file))))

      (add-hook 'merlin-mode-hook #'cb-ocaml/set-utop-command))))

(defun cb-ocaml/post-init-merlin ()
  (use-package merlin
    :bind
    (:map merlin-mode-map
          ("M-." . merlin-locate)
          ("C-c C-l" . nil))
    :evil-bind
    (:state normal :map merlin-mode-map ("M-." . merlin-locate))
    :config
    (progn
      (cb-remap-face 'merlin-type-face 'cb-faces-bg-hl-ok)
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
