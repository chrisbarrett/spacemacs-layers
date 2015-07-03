(font-lock-add-keywords
 'idris-mode
 `(("\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *=>"
    (0
     (progn (compose-region (match-beginning 1) (match-end 1)
                            ?\λ 'decompose-region)
            nil)))))
