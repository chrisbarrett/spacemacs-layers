;;; extensions.el --- cb-proof Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defvar cb-proof-pre-extensions
  '(
    proof-site
    coq
    proof-script
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-proof-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-proof/init-proof-site ()
  (use-package proof-site
    :load-path "private/cb-proof/extensions/proofgeneral/generic"
    :defer t
    :init
    (require 'proof-site)
    :config
    (progn
      (setq proof-splash-enable nil)
      (custom-set-faces
       '(proof-eager-annotation-face
         ((t (:inherit default :background nil :underline "darkgoldenrod"))))
       '(proof-error-face
         ((t (:background nil)))))

      (core/remap-face 'proof-queue-face 'core/bg-flash)
      (core/remap-face 'proof-locked-face 'core/bg-hl-ok)
      (core/remap-face 'proof-warning-face 'flycheck-warning)
      (core/remap-face 'proof-script-sticky-error-face 'flycheck-error)
      (core/remap-face 'proof-script-highlight-error-face 'flycheck-error))))

(defun cb-proof/init-coq ()
  (use-package coq
    :defer t
    :config
    (progn
      (setq coq-compile-before-require t)

      (custom-set-faces
       '(coq-cheat-face
         ((((background light)) :background "#fee8e5")
          (((background dark))  :background "#51202b")))
       `(coq-solve-tactics-face
         ((t (:italic t :foreground ,solarized-hl-orange)))))

      (add-hook 'coq-mode-hook 'coq/configure-coq-buffer)

      ;; Advices

      (defadvice coq-insert-match (after format-period activate)
        "Delete trailing whitespace until we find a period character."
        (save-excursion
          (when (search-forward "end" nil t)
            (when (search-forward-regexp (rx (group (* (or space eol)))
                                             ".")
                                         nil t)
              (replace-match "" nil nil nil 1)))))

      (defadvice coq-smie-backward-token (around ignore-errors activate)
        "Ignore bug in Coq SMIE lexer."
        (condition-case _
            ad-do-it
          (wrong-type-argument nil)))

      (evil-define-key 'normal coq-mode-map
        (kbd "S-<return>")  'proof-undo-last-successful-command
        (kbd "C-<return>")  'proof-assert-next-command-interactive
        (kbd "RET")         'proof-assert-next-command-interactive)

      (define-key coq-mode-map (kbd "S-<return>")  'proof-undo-last-successful-command)
      (define-key coq-mode-map (kbd "C-<return>")   'proof-assert-next-command-interactive)

      (define-key coq-mode-map (kbd "M-RET")   'coq/meta-ret)
      (define-key coq-mode-map (kbd "C-c C-m") 'coq-insert-match)
      (define-key coq-mode-map (kbd "RET")     'newline-and-indent))))

(defun cb-proof/init-proof-script ()
  (use-package proof-script
    :defer t
    :config
    (progn
      (define-key proof-mode-map (kbd "C-<return>") nil))))

(defun cb-proof/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode 'coq-mode
        :add '("$" "?" "^" "~" "\\")
        :custom
        '(("|" . coq/smart-pipe)
          (":" . coq/smart-colon)
          ("!" . self-insert-command)
          ("," . core/comma-then-space))))))
