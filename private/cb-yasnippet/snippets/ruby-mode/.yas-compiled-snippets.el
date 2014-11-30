;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("c" "case $1\nwhen $2\n  $0\nend" "case" nil nil nil nil nil nil)
                       ("cl" "class ${1:Name}\n  $0\nend" "class"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("d" "def ${1:fname}${2:(${3:arglist})}\n  $0\nend" "def" nil nil nil nil nil nil)
                       ("do" "do ${1:|${2:args}|}\n  $0\nend" "do" nil nil nil nil "direct-keybinding" nil)
                       ("e" "else\n" "else" nil nil nil nil nil nil)
                       ("ei" "elsif $1\n  $0" "elsif" nil nil nil nil nil nil)
                       ("i" "if $1\n  $0\nend" "if" nil nil nil nil nil nil)
                       ("main" "if __FILE__ == \\$PROGRAM_NAME\n  $0\nend\n" "main" nil nil nil nil nil nil)
                       ("mod" "module ${1:`(f-filename (f-no-ext (buffer-file-name)))`}\n  $0\nend" "module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("r" "require '$0'" "require"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("rr" "require_relative '$0'" "require_relative"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("u" "unless $1\n  $0\nend" "unless" nil nil nil nil nil nil)
                       ("w" "when $1\n  $0\nend" "when" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
