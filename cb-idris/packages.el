;;; packages.el --- cb-idris Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-idris-packages
  '(idris-mode
    smart-ops
    aggressive-indent
    (idris-autoinsert :location local)))

(defun cb-idris/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'idris-repl-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'idris-mode))))

(defun cb-idris/init-idris-mode ()
  (use-package idris-mode
    :mode "\\.idr\\'"

    :leader-bind
    (:mode
     idris-mode
     ("mr" . idris-load-file)
     ("mt" . idris-type-at-point)
     ("md" . idris-add-clause)
     ("mc" . idris-case-split)
     ("mw" . idris-make-with-block)
     ("mm" . idris-add-missing)
     ("mp" . idris-proof-search)
     ("mh" . idris-docs-at-point))

    :evil-bind
    (:state
     normal

     :map idris-info-mode-map
     ("q" . quit-window)

     :map idris-hole-list-mode-map
     ("q" . quit-window)

     :state
     insert
     ("RET" . idris/ret)
     ("SPC" . idris/smart-space)
     ("<backspace>" . idris/backspace)
     )

    :bind
    (:map
     idris-mode-map
     ("C-c C-z" . idris-switch-to-output-buffer)
     ("M-RET" . idris/meta-ret)
     :map
     idris-repl-mode-map
     ("C-c C-z" . idris/switch-to-src))

    :init
    (add-to-list 'completion-ignored-extensions ".ibc")
    :config
    (progn
      (setq idris-warnings-printing 'warnings-repl)
      (setq idris-repl-prompt-style 'long)

      (put 'idris-mode 'evil-shift-width 2)

      (defface cb-idris-function
        '((t (:bold nil :inherit font-lock-function-name-face)))
        "Face for function names in idris."
        :group 'idris)


      (cb-remap-face 'idris-semantic-type-face 'font-lock-type-face)
      (cb-remap-face 'idris-semantic-data-face 'font-lock-string-face)
      (cb-remap-face 'idris-semantic-function-face 'cb-idris-function)
      (cb-remap-face 'idris-semantic-bound-face 'font-lock-variable-name-face)
      (cb-remap-face 'idris-semantic-implicit-face 'font-lock-comment-face)
      (cb-remap-face 'idris-repl-output-face 'compilation-info)

      (add-to-list 'font-lock-keywords-alist
                   '(idris-mode
                     ((("^ *record\\>" . font-lock-keyword-face)))))

      (evil-set-initial-state 'idris-prover-script-mode 'insert)

      ;; Advice

      (defadvice idris-mode (before start-process activate)
        "Automatically run an idris process."
        (unless idris-process
          (idris-run)))

      ;; Font lock

      (defconst cb-idris/font-lock-extra-keywords
        `((,(rx (or bol space "}" ")" "]")
                (group "$" (? "!"))
                (or eol space "{" "(" "["))
           1 'font-lock-comment-face)

          (,(rx (not (any "(")) (group ",") (not (any ")")))
           1 'font-lock-comment-face)

          ,(cb-font-lock-replace-match (rx (or (and space (group-n 1 ".") space)
                                               (and "(" (group-n 1 ".") ")")
                                               ))
                                       1 "·")

          ,(cb-font-lock-replace-match (rx space (group "<-") (or space eol)) 1 "←")
          ,(cb-font-lock-replace-match (rx space (group "->") (or space eol)) 1 "→")
          ,(cb-font-lock-replace-match (rx space (group "=>") (or space eol)) 1 "⇒")

          ;; Lambda forms
          ,(cb-font-lock-replace-match
            (rx (group "\\") (and (* space)
                                  (or word "_" (and "(" (* nonl) ")"))
                                  (*? nonl))
                (* space) (or "=>" "⇒"))
            1 "λ")))

      (font-lock-add-keywords 'idris-mode cb-idris/font-lock-extra-keywords)
      (font-lock-add-keywords 'idris-repl-mode cb-idris/font-lock-extra-keywords))))

(defun cb-idris/post-init-smart-ops ()
  (defun cb-idris/looking-at-module-or-constructor? (&rest _)
    (-when-let ([fst] (thing-at-point 'symbol))
      (s-uppercase? fst)))

  (defun cb-idris/reformat-comment-at-point ()
    (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
                 (_ (equal op "{"))
                 (_ (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                                (buffer-substring beg end))))
      (goto-char beg)
      (delete-region beg end)
      (insert "{- ") (save-excursion (insert " -}"))))

  (defconst cb-idris/smart-ops
    (-flatten-n 1
                (list
                 (smart-ops "?" :pad-after nil)
                 (smart-ops "," :pad-before nil)
                 (smart-ops "$" "|" ":")
                 (smart-ops "." :pad-unless 'cb-idris/looking-at-module-or-constructor?)

                 (smart-ops "-"
                            :action #'cb-idris/reformat-comment-at-point)

                 ;; Reformat holes after `='.
                 (smart-ops "=?"
                            :pad-after nil
                            :action
                            (lambda (&rest _)
                              (save-excursion
                                (search-backward "?")
                                (just-one-space))))

                 (smart-ops-default-ops))))

  (define-smart-ops-for-mode 'idris-mode cb-idris/smart-ops)
  (define-smart-ops-for-mode 'idris-repl-mode cb-idris/smart-ops))

(defun cb-idris/init-idris-autoinsert ()
  (use-package idris-autoinsert
    :functions (idris-autoinsert-init)
    :config (idris-autoinsert-init)))
