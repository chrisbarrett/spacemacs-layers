;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("e" "echo $0" "echo"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("for" "for ${1:VAR} in ${2:str}; do\n    $0\ndone" "for"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("f" "function ${1:fname} {\n    $0\n}" "function"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("i" "if $1; then\n    $0\nfi" "if"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
