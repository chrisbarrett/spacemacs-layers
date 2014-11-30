;;; Compiled snippets and support files for `tuareg-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'tuareg-mode
                     '(("b" "begin $0\nend" "begin...end" nil nil nil nil nil nil)
                       ("ep" "eprintf \"$0 %!\"`(yas/msg \"Note that \\\"%%!\\\" causes printf to flush the channel\")`" "eprintf" nil nil nil nil nil nil)
                       ("ex" "exception ${1:exid} of ${2:type}" "exception" nil nil nil nil nil nil)
                       ("fw" "failwith \"$0\"" "failwith" nil nil nil nil nil nil)
                       ("for" "for ${1:i} = ${2:0} to ${3:bound} do\n  $0\ndone" "for" nil nil nil nil nil nil)
                       ("fp" "fprintf \"$0 %!\"`(yas/msg \"Note that \\\"%%!\\\" causes printf to flush the channel\")`\n" "fprintf" nil nil nil nil nil nil)
                       ("f" "(fun ${1:bindings} -> $0)" "fun" nil nil nil nil nil nil)
                       ("fu" "module ${1:FunctorName} (${2:M} : ${3:Constraint}) = struct\n  $0\nend\n" "functor"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("i" "if ${1:test}\nthen ${2:a}${3:\nelse ${4:b}}" "if" nil nil nil nil nil nil)
                       ("inc" "include $0" "include"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("l" "let ${1:binding} = $0\n" "let (top-level)"
                        (zerop
                         (current-indentation))
                        nil nil nil nil nil)
                       ("lo" "let open ${1:modid} in\n$0" "let open"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("lr" "let rec ${1:binding} = $0" "let rec"
                        (zerop
                         (current-indentation))
                        nil nil nil "direct-keybinding" nil)
                       ("df" "let ${1:binding} = function\n  | ${2:case} -> $0" "let with function"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("l" "let ${1:binding} = ${2:expr} in\n$0\n" "let...in"
                        (plusp
                         (current-indentation))
                        nil nil nil nil nil)
                       ("main" "(* Program entry-point *)\nlet () =\n  $0" "main"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("m" "match $0 with" "match" nil nil nil nil nil nil)
                       ("|" "| ${1:case} -> $0" "match-case" nil nil nil nil nil nil)
                       ("me" "method $1 =\n  $0" "method"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("mo" "module $0\n" "module" nil nil nil nil nil nil)
                       ("mto" "(module type of $0)" "module type of" nil nil nil nil nil nil)
                       ("mtw" "module type ${1:Module} = ${2:Module} with type ${3:t1} = ${4:t2}" "module type with"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("mu" "mutable $0" "mutable" nil nil nil nil nil nil)
                       ("ob" "object\n  $0\nend" "object" nil nil nil nil nil nil)
                       ("o" "open $0" "open"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("p" "printf \"$0 %!\"`(yas/msg \"Note that \\\"%%!\\\" causes printf to flush the channel\")`" "printf" nil nil nil nil nil nil)
                       ("protect" "protect ~f:(fun () -> ${1:expr})\n  ~finally:(fun () -> $0)" "protect" nil nil nil nil nil nil)
                       ("rec" "type ${1:name} =\n  {\n    $0\n  }" "record"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("mrec" "module ${1:modid} = struct\n  type t =\n    {\n      $0\n    }\n  with fields\nend" "record inside module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ru" "{ ${1:record} with ${2:field} = ${3:value} }" "record update" nil nil nil nil nil nil)
                       ("sig" "sig\n  $0\nend" "sig" nil nil nil nil nil nil)
                       ("msig" "module type ${1:Modname} = sig\n  type t\n  $0\nend" "signature inside module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sp" "sprintf \"$0\"`\n" "sprintf" nil nil nil nil nil nil)
                       ("st" "struct\n  $0\nend" "struct" nil nil nil nil nil nil)
                       ("try" "try\n  ${1:expr}\nwith\n  ${2:exn} -> $0" "try" nil nil nil nil nil nil)
                       ("t" "type ${1:indent} = $0\n" "type"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("mt" "module ${1:modid} = struct\n  type t = $0\nend" "type inside module"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("v" "val ${1:ident} : $0" "val"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("w" "while ${1:test} do\n  $0\ndone" "while" nil nil nil nil nil nil)
                       ("wt" "with type ${1:t1} := ${2:t2}\n" "with type" nil nil nil nil nil nil)
                       ("wf" "In_channel.with_file ${1:filename} ~f:(fun inc ->\n  $0\n)" "with_file"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
