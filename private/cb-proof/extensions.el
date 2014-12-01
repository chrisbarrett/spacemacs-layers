(defvar cb-proof-pre-extensions
  '(
    ;; pre extension cb-proofs go here
    proof-site
    coq
    proof-script
    super-smart-ops
    )
  "List of all extensions to load before the packages.")

(defvar cb-proof-post-extensions
  '(
    ;; post extension cb-proofs go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function cb-proof/init-<extension-cb-proof>
;;
;; (defun cb-proof/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-proof/init-proof-site ()
  (use-package proof-site
    :config
    (progn
      (setq proof-splash-enable nil)
      (custom-set-faces
       '(proof-eager-annotation-face
         ((t (:inherit default :background nil :underline "darkgoldenrod"))))
       '(proof-error-face
         ((t (:background nil))))
       '(proof-locked-face
         ((t (:background nil)))))


      (add-to-list 'face-remapping-alist '(proof-warning-face . flycheck-warning))
      (add-to-list 'face-remapping-alist '(proof-script-sticky-error-face . flycheck-error))
      (add-to-list 'face-remapping-alist '(proof-script-highlight-error-face . flycheck-error))

      ;; Redefine `proof-mode' to derive from `prog-mode'.
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

(defun cb-proof/init-coq ()
  (use-package coq
    :config
    (progn
      (setq coq-compile-before-require t)
      (setq coq-load-path '("src"))

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


      (define-key coq-mode-map (kbd "M-RET")   'coq/meta-ret)
      (define-key coq-mode-map (kbd "C-c C-m") 'coq-insert-match)
      (define-key coq-mode-map (kbd "M-N")     'proof-assert-next-command-interactive)
      (define-key coq-mode-map (kbd "M-P")     'proof-undo-last-successful-command)
      (define-key coq-mode-map (kbd "RET")     'newline-and-indent))))

(defun cb-proof/init-proof-script ()
  (use-package proof-script
    :config
    (progn
      (define-key proof-mode-map (kbd "C-<return>") nil))))

(defun cb-proof/init-super-smart-ops ()
  (use-package super-smart-ops
    :config
    (progn
      (super-smart-ops-configure-for-mode
       'coq-mode
       :add '("$" "?" "^" "~" "\\")
       :custom
       '(("|" . coq/smart-pipe)
         (":" . coq/smart-colon)
         ("!" . self-insert-command)
         ("," . core/comma-then-space))))))
