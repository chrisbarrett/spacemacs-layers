;;; Compiled snippets and support files for `idris-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'idris-mode
                     '(("t" "Type" "Type"
                        (or
                         (cbidris:at-data-decl\?)
                         (cbidris:at-typedecl\?))
                        nil nil nil nil nil)
                       ("d" "data ${1:ident} = ${2:Mk$1} $0" "data"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("d" "data ${1:ident} ${2:: ${3:Type}} where\n  ${4:Mk$1} : $1" "data...where"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("m" "module `(s-capitalize (f-no-ext (f-filename (buffer-file-name))))`\n\n$0\n" "module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("r" "record $0" "record"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("u" "using ($1)\n  $0" "using" nil nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("w" "where" "where"
                        (cbidris:at-data-decl\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
