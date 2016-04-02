;;; config.el --- config for cb-smartparens
;;; Commentary:
;;; Code:

(require 'dash)
(require 'cb-vars)

(autoload 'sp-navigate-reindent-after-up "smartparens")
(autoload 'smartparens-mode "smartparens")
(autoload 'sp-with-modes "smartparens")
(autoload 'sp-local-pair "smartparens")

(with-eval-after-load 'smartparens

  ;;; Remove apostrophe pair for some modes

  (sp-with-modes cb-vars-prompt-modes
    (sp-local-pair "'" "'" :actions '(:rem insert)))

  (sp-local-pair 'org-mode                 "'" "'" :actions '(:rem insert))
  (sp-local-pair 'extempore-mode           "'" "'" :actions '(:rem insert))
  (sp-local-pair 'text-mode                "'" "'" :actions '(:rem insert))
  (sp-local-pair 'minibuffer-inactive-mode "'" "'" :actions '(:rem insert))

  
  ;;; Org

  (add-hook 'org-agenda-mode-hook (lambda () (smartparens-mode -1)))

  (sp-with-modes 'org-mode
    (sp-local-pair "\\[" "\\]" :post-handlers '(:add sp-internal-and-external-padding))
    )

  ;;; Scala

  (sp-with-modes 'scala-mode
    (sp-local-pair "{" "}" :post-handlers '(:add sp/scala-curly-brace-padding))
    (sp-local-pair "(" ")" :post-handlers '(:add sp/scala-format-after-paren))
    )

  ;;; Json/JavaScript

  (sp-with-modes '(js-mode js2-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding))
    )

  ;;; Haskell

  (sp-with-modes '(haskell-mode inf-haskell-mode haskell-interactive-mode)
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "{" "}" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "(" ")" :post-handlers '(:add sp/haskell-external-padding))
    (sp-local-pair "[" "]" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "{-@" "@-}" :post-handlers '(:add sp-internal-and-external-padding))
    (sp-local-pair "{-#" "#-}" :post-handlers '(:add sp-internal-and-external-padding))
    (sp-local-pair "`" "`" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "'" "'" :actions '(:rem insert))
    )

  ;;; OCaml

  (sp-with-modes '(tuareg-mode utop-mode sml-mode lazy-sml-mode inferior-sml-mode)
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "[|" "|]" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "{<" ">}" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "'" "'"   :actions '(:rem insert))
    (sp-local-pair "`" nil   :actions nil)
    )

  ;;; Coq

  (sp-with-modes 'coq-mode
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "'" "'"   :actions '(:rem insert))
    )

  ;;; F#

  (sp-with-modes 'fsharp-mode
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "[|" "|]" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "[<" ">]" :post-handlers '(:add sp/ml-just-one-space))
    (sp-local-pair "'" "'"   :actions '(:rem insert))
    (sp-local-pair "`" nil   :actions nil)
    )

  ;;; Idris

  (sp-with-modes '(idris-mode idris-repl-mode)
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp-internal-and-external-padding))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "`" "`"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "[|" "|]" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "'" nil   :actions nil)
    (sp-local-pair "'" "'"   :actions '(:rem insert))
    )

  ;;; Agda

  (sp-with-modes '(agda2-mode)
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "`" "`"   :post-handlers '(:add sp-external-padding))
    (sp-local-pair "[|" "|]" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "'" nil   :actions nil)
    (sp-local-pair "'" "'"   :actions '(:rem insert))
    )


  ;;; Lisp modes

  (sp-with-modes cb-vars-lisp-modes
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp/lisp-just-one-space))
    (sp-local-pair "{" "}"   :post-handlers '(:add sp/lisp-just-one-space))
    (sp-local-pair "[" "]"   :post-handlers '(:add sp/lisp-just-one-space))
    (sp-local-pair "(" ")"   :post-handlers '(:add sp/lisp-just-one-space))
    (sp-local-pair "'" nil   :actions nil)
    )

  ;;; Eshell

  (sp-with-modes 'eshell-mode
    (sp-local-pair "\"" "\"" :post-handlers)
    (sp-local-pair "{" "}"   :post-handlers)
    (sp-local-pair "[" "]"   :post-handlers)
    (sp-local-pair "(" ")"   :post-handlers)
    (sp-local-pair "'" nil   :actions nil)
    )

  ;; Extend `sp-navigate-reindent-after-up' to all lisps.
  (let ((ls (assoc 'interactive sp-navigate-reindent-after-up)))
    (setcdr ls (-uniq (-concat (cdr ls) cb-vars-lisp-modes))))

  ;;; Markdown

  (sp-with-modes 'markdown-mode
    (sp-local-pair "```" "```"))

  ;;; Python

  (sp-with-modes '(python-mode inferior-python-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add sp-external-padding)))

  ;;; Ruby

  (require 'smartparens-ruby)

  (with-eval-after-load 'ruby-mode
    (modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?! "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?? "w" ruby-mode-syntax-table))

  (sp-with-modes '(ruby-mode inf-ruby-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding))
    (sp-local-pair "[" "]" :pre-handlers '(sp-ruby-pre-handler))

    (sp-local-pair "%q{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%Q{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%w{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%W{" "}" :when '(sp-in-code-p))
    (sp-local-pair  "%(" ")" :when '(sp-in-code-p))
    (sp-local-pair "%x(" ")" :when '(sp-in-code-p))
    (sp-local-pair  "#{" "}" :when '(sp-in-string-p))

    (sp-local-pair "|" "|"
                   :when '(sp/ruby-should-insert-pipe-close)
                   :unless '(sp-in-string-p)
                   :pre-handlers '(sp/ruby-sp-hook-space-before)
                   :post-handlers '(sp/ruby-sp-hook-space-after))

    (sp-local-pair "case" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-ruby-in-string-or-word-p)
                   :actions '(insert)
                   :pre-handlers '(sp-ruby-pre-handler)
                   :post-handlers '(sp-ruby-block-post-handler)))

  ;;; Swift

  (sp-with-modes '(swift-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding))
    (sp-local-pair "'" "'"   :actions '(:rem insert)))

  ;;; C

  (sp-with-modes '(c-mode cc-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add sp-external-padding))
    (sp-local-pair "(" ")" :post-handlers '(:add sp/c-format-after-paren)))

  (sp-with-modes 'c++-mode
    (sp-local-pair "{" "}" :post-handlers '(sp/c++-format-after-open-curly))
    (sp-local-pair "(" ")" :post-handlers '(:add sp/c-format-after-paren))
    )

  )

;;; config.el ends here
