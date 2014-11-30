;;; Compiled snippets and support files for `snippet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippet-mode
                     '(("c" "# condition: (yas/bol?)" "bol condition" nil nil nil nil "direct-keybinding" nil)
                       ("ee" "# expand-env: ((yas-indent-line 'fixed) (yas-wrap-around-region 't))" "expand-env"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
