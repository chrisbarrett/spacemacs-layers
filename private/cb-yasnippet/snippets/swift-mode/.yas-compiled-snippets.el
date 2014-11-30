;;; Compiled snippets and support files for `swift-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'swift-mode
                     '(("c" "case $0" "case"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("cl" "class ${1:Name} {\n  $0\n}" "class"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("dw" "do {\n  $0\n} while $1" "do-while"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("e" "else {\n  $0\n}" "else"
                        (or
                         (yas/bol\?)
                         (thing-at-point-looking-at
                          (rx "}"
                              (* space)
                              "e")))
                        nil nil nil "direct-keybinding" nil)
                       ("en" "enum ${1:Name} {\n  $0\n}" "enum"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("for" "for var ${1:x} = 0; $1 != ${2:limit}; ++$1 {\n    $0\n}\n" "for"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("fi" "for $1 in $2 {\n    $0\n}" "for-in"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("f" "func ${1:name} (${2:args}) -> ${3:ret} {\n  $0\n}" "func"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("i" "if $1 {\n  $0\n}" "if"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("l" "let $0" "let"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("st" "struct ${1:Name} {\n  $0\n}" "struct"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("sw" "switch ${1:x} {\ncase ${2:c}:\n     $0\n}" "switch"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("wh" "while $1 {\n  $0\n}" "while"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
