;;; Compiled snippets and support files for `js2-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js2-mode
                     '(("e" "else {\n    $0\n}\n" "else"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("ei" "else if (${1:test}) {\n    $0\n}" "else if"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("f" "function ${1:name}(${2:args...}) { $0 }" "function" nil nil nil nil "direct-keybinding" nil)
                       ("i" "if (${1:test}) {\n    $0\n}" "if"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("i" "if (${1:test}) {\n    ${2:then}\n}\nelse {\n    ${3:else}\n}" "if...else"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("r" "return $0;" "return" nil nil nil nil "direct-keybinding" nil)
                       ("v" "var ${1:name} = $0;" "var"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("wh" "while (${1:test}) {\n    $0\n}" "while"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
