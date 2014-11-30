;;; Compiled snippets and support files for `fsharp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'fsharp-mode
                     '(("fw" "failwith \"$0\"" "failwith" nil nil nil nil nil nil)
                       ("f" "(fun ${1:bindings} -> $0)" "fun" nil nil nil nil nil nil)
                       ("i" "if ${1:test}\nthen ${2:a}${3:\nelse ${4:b}}" "if" nil nil nil nil nil nil)
                       ("l" "let ${1:binding} = $0\n" "let"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("df" "let ${1:binding} = function\n  | ${2:case} -> $0" "let with function"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("m" "match $0 with" "match" nil nil nil nil nil nil)
                       ("|" "| ${1:case} -> $0" "match-case" nil nil nil nil nil nil)
                       ("mo" "module $0" "module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("mu" "mutable $0" "mutable" nil nil nil nil nil nil)
                       ("o" "open $0" "open"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("p" "printfn \"${1:%A}\" $0\n" "printf" nil nil nil nil nil nil)
                       ("rec" "type ${1:name} =\n  {\n    $0\n  }" "record"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ru" "{ ${1:record} with ${2:field} = ${3:value} }" "record update" nil nil nil nil nil nil)
                       ("try" "try\n  ${1:expr}\nwith\n  ${2:exn} -> $0" "try" nil nil nil nil nil nil)
                       ("t" "type $0" "type"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("w" "while ${1:test} do\n  $0" "while" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
