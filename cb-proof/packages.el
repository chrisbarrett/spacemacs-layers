;;; extensions.el --- cb-proof Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'f)

(autoload 'evil-define-key "evil-core")
(autoload 'core/remap-face "config")

(defconst cb-proof-packages
  '(flycheck
    smart-ops
    aggressive-indent
    (proof-site :location local)
    (coq :location local)
    (proof-script :location local)
    (coq-meta-ret :location local)
    (coq-unicode :location local)))

(add-to-list 'load-path (f-join (with-no-warnings user-layers-directory)
                                "cb-proof/local/PG/generic"))

(defun cb-proof/init-proof-site ()
  (require 'proof-site)


  (with-no-warnings
    (setq proof-splash-enable nil))

  (custom-set-faces
   '(proof-eager-annotation-face
     ((t (:inherit default :background nil :underline "darkgoldenrod"))))
   '(proof-error-face
     ((t (:background nil)))))

  (core/remap-face 'proof-queue-face 'cb-faces-bg-flash)
  (core/remap-face 'proof-locked-face 'cb-faces-bg-hl-ok)
  (core/remap-face 'proof-warning-face 'flycheck-warning)
  (core/remap-face 'proof-script-sticky-error-face 'flycheck-error)
  (core/remap-face 'proof-script-highlight-error-face 'flycheck-error))

(defun cb-proof/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (with-eval-after-load 'aggressive-indent
      (add-to-list 'aggressive-indent-excluded-modes 'coq-mode))))

(defun cb-proof/init-coq ()
  (use-package coq
    :defer t
    :config
    (progn
      (with-no-warnings
        (setq coq-compile-before-require t))

      (custom-set-faces
       '(coq-cheat-face
         ((((background light)) :background "#fee8e5")
          (((background dark))  :background "#51202b")))
       `(coq-solve-tactics-face
         ((t (:italic t :foreground ,(with-no-warnings solarized-hl-orange))))))

      (defun cb-proof--configure-coq-buffer ()
        (setq-local compile-command (concat "coqc " (buffer-name))))

      (add-hook 'coq-mode-hook #'cb-proof--configure-coq-buffer)

      (defun cb-proof--delete-trailing-whitespace (&rest _)
        (save-excursion
          (when (search-forward "end" nil t)
            (when (search-forward-regexp (rx (group (* (or space eol)))
                                             ".")
                                         nil t)
              (replace-match "" nil nil nil 1)))))

      (advice-add 'coq-insert-match :after #'cb-proof--delete-trailing-whitespace)

      (defun cb-proof--ignore-smie-error (oldfn &rest args)
        (condition-case _
            (apply oldfn args)
          (wrong-type-argument nil)))

      (advice-add 'coq-smie-backward-token :around #'cb-proof--ignore-smie-error)

      (let ((map (with-no-warnings coq-mode-map)))
        (evil-define-key 'normal map
          (kbd "S-<return>") #'proof-undo-last-successful-command
          (kbd "C-<return>") #'proof-assert-next-command-interactive
          (kbd "RET")        #'proof-assert-next-command-interactive)

        (define-key map (kbd "S-<return>") #'proof-undo-last-successful-command)
        (define-key map (kbd "C-<return>") #'proof-assert-next-command-interactive)
        (define-key map (kbd "C-c C-m")    #'coq-insert-match)
        (define-key map (kbd "RET")        #'newline-and-indent)))))

(defun cb-proof/init-proof-script ()
  (use-package proof-script
    :defer t
    :config
    (progn
      (define-key proof-mode-map (kbd "C-<return>") nil)

      ;; HACK: Redefine `proof-mode' to derive from `prog-mode'.
      (define-derived-mode proof-mode prog-mode
        proof-general-name
        "Proof General major mode class for proof scripts.
\\{proof-mode-map}"

        (setq proof-buffer-type 'script)

        ;; Set default indent function (can be overriden in derived modes)
        (make-local-variable 'indent-line-function)
        (setq indent-line-function 'proof-indent-line)

        ;; During write-file it can happen that we re-set the mode for the
        ;; currently active scripting buffer.  The user might also do this
        ;; for some reason.  We could maybe let this pass through, but it
        ;; seems safest to treat it as a kill buffer operation (retract and
        ;; clear spans).  NB: other situations cause double calls to proof-mode.
        (if (eq (current-buffer) proof-script-buffer)
            (proof-script-kill-buffer-fn))

        ;; We set hook functions here rather than in proof-config-done so
        ;; that they can be adjusted by prover specific code if need be.
        (proof-script-set-buffer-hooks)

        ;; Set after change functions
        (proof-script-set-after-change-functions)

        (add-hook 'after-set-visited-file-name-hooks
                  'proof-script-set-visited-file-name nil t)

        (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)))))

(defun cb-proof/post-init-smart-ops ()
  (define-smart-ops-for-mode 'coq-mode
    (smart-ops ":"
               :pad-before-unless
               (smart-ops-after-match? (rx bow "eqn" (* space) eos)))
    (smart-ops "|"
               :action
               (lambda ()
                 (when (sp-inside-square-braces?)
                   (delete-horizontal-space)
                   (insert " ")
                   (save-excursion
                     (insert " |")))))

    (smart-ops "?" "^" "~" "\\")
    (smart-ops-default-ops)))

(defun cb-proof/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (progn
      (defun cb-proof--disable-coq-checker ()
        (add-to-list 'flycheck-disabled-checkers 'coq))
      (add-hook 'flycheck-mode-hook #'cb-proof--disable-coq-checker))))

(defun cb-proof/init-coq-meta-ret ()
  (use-package coq-meta-ret
    :functions (coq-meta-ret-init)
    :config (coq-meta-ret-init)))

(defun cb-proof/init-coq-unicode ()
  (use-package coq-unicode
    :functions (coq-unicode-init)
    :config (coq-unicode-init)))

;;; packages.el ends here
