;;; Compiled snippets and support files for `haskell-c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-c-mode
                     '(("e" "#{enum ${1:`(or (cb-hs:last-declared-type-name) \"type\")`}, $1\n , $0\n }\n" "#{enum...}"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        nil nil nil)
                       ("inc" "#include <$0>" "include"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("st" "#starttype ${1:structname}\n#field ${2:fields , types...}\n#stoptype" "#starttype...#stoptype"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
