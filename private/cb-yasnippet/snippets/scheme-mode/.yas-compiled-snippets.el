;;; Compiled snippets and support files for `scheme-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'scheme-mode
                     '(("case" "(case $1\n  ($0))\n" "case" nil nil nil nil nil nil)
                       ("cond" "(cond\n  ($1)\n  (else $0))\n" "cond" nil nil nil nil nil nil)
                       ("cust" "(define ${1:cust} (make-custodian))\n(parameterize ([current-custodian $1])\n  $0\n  )\n" "cust" nil nil nil nil nil nil)
                       ("d" "(define ${1:name} ${2:val})" "define" nil nil nil nil "direct-keybinding" nil)
                       ("df" "(define ($1)\n  $0)\n" "defun" nil nil nil nil nil nil)
                       ("i" "(if $1\n  $2\n  $0)\n" "if" nil nil nil nil nil nil)
                       ("\\" "(lambda ($1) $0)\n" "lambda" nil nil nil nil nil nil)
                       ("l" "(let (($1))\n  $0)\n" "let" nil nil nil nil nil nil)
                       ("l*" "(let* (($1))\n  $0)\n" "let*" nil nil nil nil nil nil)
                       ("lr" "(letrec (($1))\n  $0)\n" "letrec" nil nil nil nil nil nil)
                       ("thread" "(thread (lambda ()\n         $0))\n" "thread" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
