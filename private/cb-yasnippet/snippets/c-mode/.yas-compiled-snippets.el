;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
                     '(("dw" "do\n{\n  $0\n}\nwhile (${1:condition});" "do while" nil nil nil nil nil nil)
                       ("e" "else\n{\n    $0\n}" "else"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("f" "for (${1:int i = 0}; ${2: i != ${3:n}}; ${4:i++})\n{\n    $0\n}" "for"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("guard" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}\n#define $1\n\n$0\n\n#endif /* $1 */\n" "guard" nil nil nil nil nil nil)
                       ("i" "if (${1:condition})\n{\n  $0\n}" "if"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ifdef" "#ifdef $1\n$0\n#endif" "ifdef" nil nil nil nil nil nil)
                       ("ifndef" "#ifndef $1\n$0\n#endif" "ifndef" nil nil nil nil nil nil)
                       ("main" "int main(${1:int argc, char *argv[]})\n{\n  $0\n  return 0;\n}" "main"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("math" "#include <math.h>\n$0" "math" nil nil nil nil nil nil)
                       ("p" "printf(\"${1:%s}\"$2);$0" "printf"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("st" "static $0" "static" nil nil nil nil nil nil)
                       ("stdio" "#include <stdio.h>\n$0" "stdio" nil nil nil nil nil nil)
                       ("stdlib" "#include <stdlib.h>\n$0" "stdlib" nil nil nil nil nil nil)
                       ("s" "struct ${1:name}\n{\n  $0\n};" "struct" nil nil nil nil nil nil)
                       ("td" "typedef $0" "typedef" nil nil nil nil nil nil)
                       ("tds" "typedef struct $1\n{\n  $0\n} $1;\n" "typedef struct" nil nil nil nil nil nil)
                       ("un" "union\n{\n  $0\n} ${1:name};" "union" nil nil nil nil nil nil)
                       ("wh" "while (${1:condition})\n{\n    $0\n}" "while"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
