;;; Compiled snippets and support files for `rust-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rust-mode
                     '(("#a" "#[allow(dead_code)]" "allow dead_code" nil nil nil nil nil nil)
                       ("#c" "#[cfg(${1:parameter} = ${2:val})]" "cfg value" nil nil nil nil nil nil)
                       ("#c" "#[cfg(${1:flag})]" "cfg flag" nil nil nil nil nil nil)
                       ("#c" "#[cfg(${1:parameter} = ${2:val})]" "cfg value" nil nil nil nil nil nil)
                       ("c" "|${1:args...}| {\n    $0\n}" "closure" nil nil nil nil "direct-keybinding" nil)
                       ("#!" "#![allow(dead_code)]" "crate allow(dead_code)" nil nil nil nil nil nil)
                       ("#!" "#![crate_${1:attrname} = ${2:val}]" "crate_attribute" nil nil nil nil "direct-keybinding" nil)
                       ("#d" "#[deriving($0)]" "deriving attribute" nil nil nil nil nil nil)
                       ("e" "else {\n    $0\n}" "else"
                        (yas/line-matches-up-to-point\?
                         (rx bol
                             (* space)
                             (32 "}")
                             (* space)
                             eol))
                        nil nil nil nil nil)
                       ("ei" "else if ${1:expr} {\n  $0\n}" "else-if" nil nil nil nil "direct-keybinding" nil)
                       ("e" "enum ${1:Name} {\n    $0\n}" "enum"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil "direct-keybinding" nil)
                       ("ec" "extern crate ${1:name};" "extern crate"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("f" "fn ${1:name}(${2:args})${3: -> ${4:T}} {\n    $0\n}" "fn"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil nil nil)
                       ("for" "for ${1:x} in ${2:expr} {\n    $0\n}" "for"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("fmt" "format!(\"$1\"${1:$(cbrs:fmt-println-args yas/text)});" "format!" nil nil nil nil "direct-keybinding" nil)
                       ("i" "if ${1:pred} {\n    $0\n}" "if"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("i" "if ${1:pred} {\n    ${2:expr}\n} else {\n    ${3:expr}\n}" "if...else" nil nil nil nil nil nil)
                       ("im" "impl ${1:`(cbrs:previous-struct-def)`} {\n    $0\n}" "impl" nil nil nil nil "direct-keybinding" nil)
                       ("l" "let ${1:x} = $0;" "let"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("lm" "let mut ${1:x} = $0;" "let mut"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("m" "match ${1:expr} {\n    ${2:x} => $0,\n    _ =>\n}" "match" nil nil nil nil nil nil)
                       ("mod" "mod ${1:name} {\n    $0\n}" "module"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil nil nil)
                       ("p" "println!(\"$1\"${1:$(cbrs:fmt-println-args yas/text)});" "println!"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("pf" "pub fn ${1:name}(${2:args})${3: -> ${4:T}} {\n    $0\n}" "public function"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil "direct-keybinding" nil)
                       ("sig" "fn ${1:name}(${2:&self})${3: -> ${4:T}};" "signature in trait"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil "direct-keybinding" nil)
                       ("st" "static ${1:NAME$(upcase yas/text)} : ${2:type} = ${3:val};" "static variable"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil "direct-keybinding" nil)
                       ("s" "struct ${1:Name} {\n     $0\n}" "struct"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("tr" "trait ${1:Name} {\n    $0\n}" "trait"
                        (cbrs:bol-or-after-accessibility-modifier\?)
                        nil nil nil "direct-keybinding" nil)
                       ("t" "type ${1:alias} = ${2:name};" "type"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("u" "use ${1:ident};" "use"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("wh" "while ${1:pred} {\n    $0\n}" "while"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
