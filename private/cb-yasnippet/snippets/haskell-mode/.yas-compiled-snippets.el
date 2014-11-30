;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("c" "case ${1:expr} of\n  ${2:ctor} -> $0" "case"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("d" "data ${1:Name} = ${2:$1} $0\n  ${3:deriving (${4:Show, Eq})}" "data"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil nil nil)
                       ("d" "data ${1:Name} = ${2:$1}\n  { ${5:member} :: ${6:type}\n  , ${7:member} :: ${8:type} $0\n  } ${3:deriving (${4:Show, Eq})}" "data record"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("di" "deriving instance $0" "deriving instance"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("do-binding" "${1:x} <- $0\n" "do-binding" nil nil nil nil nil nil)
                       ("fi" "foreign import ccall ${1:unsafe }\"${2:header} ${3:function}\"\n  ${4:c_$3} :: $0\n`(cb-hs:insert-import \"Foreign\")`\n`(cb-hs:insert-import \"Foreign.C.Types\")`\n" "foreign import"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("i" "if ${1:pred}\nthen ${2:expr}\nelse ${3:expr}" "if" nil nil nil nil "direct-keybinding" nil)
                       ("in" "instance ${2:class} where\n  $0" "instance"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("\\" "\\\\${1:x} -> ${2:expr}$0" "lambda" nil nil nil nil "direct-keybinding" nil)
                       ("l" "let ${1:x} = ${2:expr}$0" "let" nil nil nil nil "direct-keybinding" nil)
                       ("mc" "${1:pat} -> $0" "match-case" nil nil nil nil nil nil)
                       ("nt" "newtype ${1:name} = $1 { un$1 :: ${2:type} }\n  ${3:deriving (${4:Show, Eq})}" "newtype"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil nil nil)
                       ("p" "putStrLn $0" "putStrLn" nil nil nil nil nil nil)
                       ("t" "type ${1:name} = $0" "type"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("tf" "type family ${1:decl} :: ${2:type}" "type family"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ti" "type instance $0" "type instance"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("tc" "class $1 where\n  $0" "typeclass"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("u" "undefined" "undefined" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
