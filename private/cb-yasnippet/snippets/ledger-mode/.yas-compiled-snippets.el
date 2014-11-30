;;; Compiled snippets and support files for `ledger-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ledger-mode
                     '(("a" "Assets:${1:Checking}$0" "Assets" nil nil nil nil nil nil)
                       ("comment" "comment\n  $0\nend comment" "comment" nil nil nil nil nil nil)
                       ("e" "Expenses:$0\n" "expense" nil nil nil nil nil nil)
                       ("i" "Income:${1:ACCOUNT}                              \\$ -${2:AMOUNT}" "income"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("re" "Assets:Reimbursements:${1:NAME}           \\$ ${2:VALUE}\nAssets:${3:Checking}" "reimbursement" nil nil nil nil nil nil)
                       ("rep" "Assets:${1:Checking}                     \\$ ${2:VALUE}\nAssets:Reimbursements:${3:NAME}" "repayment" nil nil nil nil nil nil)
                       ("t" "`(s-replace \"-\" \"/\" (org-read-date ))` ${2:(${3:REF}) }${4:PAYEE}\n  $0" "transaction" nil nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
