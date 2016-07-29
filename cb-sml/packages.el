;;; packages.el --- cb-sml Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(autoload 'evil-local-set-key "evil-core")
(autoload 'flycheck-buffer "flycheck")

(defconst cb-sml-packages
  '(sml-mode
    smart-ops
    aggressive-indent
    (flycheck-sml :location local)
    (lazy-sml-mode :location local)))

(defun cb-sml/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'sml-mode)))

(defun cb-sml/post-init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :commands (run-sml sml-mode)

    :bind
    (:map
     sml-mode-map
     ("M-RET" . cb-sml/m-ret)
     ("S-TAB" . sml-back-to-outer-indent)
     ("<return>" . cb-sml/ret)
     ("M-SPC" . nil)
     :map
     inferior-sml-mode-map
     ("M-RET" . cb-sml/inf-sml-m-ret))

    :evil-bind
    (:map sml-mode-map :state normal (",ct" . flycheck-buffer))

    :config
    (progn
      (setq sml-indent-level 2)

      (defadvice sml-prog-proc-switch-to (after append-buffer activate)
        (goto-char (point-max))
        (when (thing-at-point-looking-at sml-prompt-regexp)
          (evil-insert-state)))

      ;; Use font locking to display pretty arrows.

      (font-lock-add-keywords
       'sml-mode
       `(,(cb-font-lock-replace-match (rx space (group "->") space) 1 "→")
         ,(cb-font-lock-replace-match (rx space (group "=>") space) 1 "⇒")))

      ;; Advice to work around super-aggressive SML indentation.

      (defun cb-sml/reindent-for-evil-open (f &rest args)
        (let ((col (current-indentation)))
          (apply f args)
          (when (and (derived-mode-p 'sml-mode) (s-blank? (s-trim (cb-buffers-current-line))))
            (delete-horizontal-space)
            (indent-to col))))

      (advice-add 'evil-open-below :around #'cb-sml/reindent-for-evil-open)
      (advice-add 'evil-open-above :around #'cb-sml/reindent-for-evil-open)

      ;; Hack SMIE indentation so that structures are indented.

      (defun cb-sml/smie-rules (orig kind token)
        (pcase (cons kind token)
          (`(:after . "struct") 2)
          (_ (funcall orig kind token))))

      (advice-add 'sml-smie-rules :around #'cb-sml/smie-rules))))

(defun cb-sml/post-init-smart-ops ()
  (require 'smart-ops)
  (let ((ops (-flatten-n 1 (list
                            (smart-ops "," ";" :pad-before nil)
                            (smart-ops "*" "^" "@")
                            (smart-ops-default-ops)))))
    (define-smart-ops-for-mode 'sml-mode ops)
    (define-smart-ops-for-mode 'lazy-sml-mode ops)
    (define-smart-ops-for-mode 'inferior-sml-mode ops))

  (add-hook 'inferior-sml-mode-hook #'smart-ops-mode))

(defun cb-sml/init-flycheck-sml ()
  (use-package flycheck-sml))

(defun cb-sml/init-lazy-sml-mode ()
  (use-package lazy-sml-mode
    :mode ("\\.lml\\'" . lazy-sml-mode)))

;;; packages.el ends here
