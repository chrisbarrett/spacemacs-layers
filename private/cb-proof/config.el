;; Font lock

(dolist (mode '(coq-mode coq-response-mode coq-goals-mode))
  (font-lock-add-keywords
   mode
   (list
    (core/font-lock-replace-match (rx (and (or bol (any "," ":" "(" "[" ">")) (* space)) bow (group "forall") eow) 1 (string-to-char "∀"))
    (core/font-lock-replace-match (rx (and (or bol (any "," ":" "(" "[" ">")) (* space)) bow (group "exists") eow) 1 (string-to-char "∃"))
    (core/font-lock-replace-match (rx (or space eow) (group "/\\") (or space eol bow)) 1 (string-to-char "∧"))
    (core/font-lock-replace-match (rx (or space eow) (group "\\/") (or space eol bow)) 1 (string-to-char "∨"))
    (core/font-lock-replace-match (rx (or space eow) (group "->")  (or space eol bow)) 1 (string-to-char "→"))
    (core/font-lock-replace-match (rx (or space eow) (group "<->") (or space eol bow)) 1 (string-to-char "↔"))
    (core/font-lock-replace-match (rx (or space eow) (group "<-")  (or space eol bow)) 1 (string-to-char "←"))
    (core/font-lock-replace-match (rx (or space eow) (group "~")   (or space eol bow)) 1 (string-to-char "¬"))
    (core/font-lock-replace-match (rx (or space eow) (group "=>")  (or space eol bow)) 1 (string-to-char "⇒"))
    (core/font-lock-replace-match (rx (or space eow) (group "<>")  (or space eol bow)) 1 (string-to-char "≠"))
    (core/font-lock-replace-match (rx (or space eow) (group ">=")  (or space eol bow)) 1 (string-to-char "≥"))
    (core/font-lock-replace-match (rx (or space eow) (group "<=")  (or space eol bow)) 1 (string-to-char "≤"))
    (core/font-lock-replace-match (rx (or space eow) (group "|-")  (or space eol bow)) 1 (string-to-char "⸠"))
    )))


;; Redefine `proof-mode' to derive from `prog-mode'.
(after 'proof-script
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

    (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)))
