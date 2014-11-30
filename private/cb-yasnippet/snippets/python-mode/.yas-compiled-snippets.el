;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("c" "class ${1:$$(s-upper-camel-case yas/text)}${2: ($3)}:\n\n    ${4:'''$5'''\n\n    }$0\n" "class"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("d" "def ${1:$$(s-downcase yas/text)} ($2):\n    ${3:\"\"\"$4\n    ${2:$(yas/python-docstring yas/text)}\n\n    ${5:Returns:\n    $6\n\n    }\"\"\"\n    }$0\n" "def"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("e" "else:\n" "else"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("f" "for ${1:x} in ${2:xs}:\n    $0\n" "for"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("fr" "from $1 import ${2:*}\n" "from"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("i" "if $1:\n    $0\n" "if"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("im" "import $0\n" "import"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("init" "\ndef __init__ (self${1:, $2}):\n    ${3:\"\"\"$4\n    ${2:$(yas/python-docstring yas/text)}\n\n    \"\"\"\n    }$0\n" "init"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("lc" "[${1:x} for ${2:x} in ${3:xs}]\n$0" "list comprehension" nil nil nil nil nil nil)
                       ("main" "if __name__ == '__main__':\n    $0\n" "main"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("m" "def ${1:$$(s-downcase yas/text)} (self${2:, $3}):\n    ${4:\"\"\"$5\n    ${3:$(yas/python-docstring yas/text)}\n\n    ${6:Returns:\n    $7\n\n    }\"\"\"\n    }$0\n" "method"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("new" "def __new__(mcs, name, bases, dict):\n    $0\n    return type.__new__(mcs, name, bases, dict)\n" "new"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("p" "print($0)\n" "print"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("r" "return $0\n" "return"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sc" "{${1:x} for ${2:x} in ${3:xs}}\n$0" "set comprehension" nil nil nil nil nil nil)
                       ("t" "try:\n    ${1:expr}\nexcept ${2:Exception}:\n    $0\n" "try"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("wh" "while ${1:True}:\n    $0\n" "while"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("wi" "with ${1:expr}${2: as ${3:alias}}:\n    $0\n" "with"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
