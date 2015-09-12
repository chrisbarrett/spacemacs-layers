;;; packages.el --- cb-idris Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-idris-packages
  '(idris-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-idris-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-idris/init-idris-mode ()
  (use-package idris-mode
    :mode "\\.idr\\'"
    :init
    (add-to-list 'completion-ignored-extensions ".ibc")
    :config
    (progn
      (with-eval-after-load 'aggressive-indent
        (add-to-list 'aggressive-indent-excluded-modes 'idris-repl-mode)
        (add-to-list 'aggressive-indent-excluded-modes 'idris-mode))

      (setq idris-warnings-printing 'warnings-repl)
      (setq idris-repl-prompt-style 'long)

      (put 'idris-mode 'evil-shift-width 2)

      (defface cb-idris-function
        '((t (:bold nil :inherit font-lock-function-name-face)))
        "Face for function names in idris."
        :group 'idris)


      (core/remap-face 'idris-semantic-type-face 'font-lock-type-face)
      (core/remap-face 'idris-semantic-data-face 'font-lock-string-face)
      (core/remap-face 'idris-semantic-function-face 'cb-idris-function)
      (core/remap-face 'idris-semantic-bound-face 'font-lock-variable-name-face)
      (core/remap-face 'idris-semantic-implicit-face 'font-lock-comment-face)
      (core/remap-face 'idris-repl-output-face 'compilation-info)

      (add-to-list 'font-lock-keywords-alist
                   '(idris-mode
                     ((("^ *record\\>" . font-lock-keyword-face)))))

      ;; Key bindings

      (evil-leader/set-key-for-mode 'idris-mode
        "mr" 'idris-load-file
        "mt" 'idris-type-at-point
        "md" 'idris-add-clause
        "mc" 'idris-case-split
        "mw" 'idris-make-with-block
        "mm" 'idris-add-missing
        "mp" 'idris-proof-search
        "mh" 'idris-docs-at-point)

      (evil-define-key 'normal idris-info-mode-map (kbd "q") 'quit-window)
      (evil-define-key 'normal idris-hole-list-mode-map (kbd "q") 'quit-window)

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

          ,(core/font-lock-replace-match (rx (or (and space (group-n 1 ".") space)
                                                 (and "(" (group-n 1 ".") ")")
                                                 ))
                                         1 "·")

          ,(core/font-lock-replace-match (rx space (group "<-") (or space eol)) 1 "←")
          ,(core/font-lock-replace-match (rx space (group "->") (or space eol)) 1 "→")
          ,(core/font-lock-replace-match (rx space (group "=>") (or space eol)) 1 "⇒")

          ;; Lambda forms
          ,(core/font-lock-replace-match
            (rx (group "\\") (and (* space)
                                  (or word "_" (and "(" (* nonl) ")"))
                                  (*? nonl))
                (* space) (or "=>" "⇒"))
            1 "λ")))

      (font-lock-add-keywords 'idris-mode cb-idris/font-lock-extra-keywords)
      (font-lock-add-keywords 'idris-repl-mode cb-idris/font-lock-extra-keywords))))
