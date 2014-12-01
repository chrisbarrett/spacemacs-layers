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
