;;; Compiled snippets and support files for `bison-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'bison-mode
                     '(("d" "%define api.value.type {$0}" "%define"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("e" "%empty $0" "%empty" nil nil nil nil nil nil)
                       ("l" "%left $0" "%left"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("pr" "%precedence $0" "%precedence"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("r" "%right $0" "%right"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("t" "%token $0" "%token"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("p" "${1:name}:\n  $0\n;" "production"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
