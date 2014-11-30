;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("sum" "#+TBLFM: @>$${1:column number}=vsum(@I..@II)\n" "Table Sum Column" nil nil nil nil nil nil)
                       ("ex" "#+begin_example\n$0\n#+end_example" "begin_example"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("q" "#+begin_quote\n$0\n#+end_quote" "begin_quote"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ditaa" "#+begin_src ditaa :file ${1:filename}.png\n$0\n#+end_src" "ditaa"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("frag" "#+ATTR_REVEAL: :frag `(cb-org:reveal-read-frag-style)`\n" "frag" nil nil nil nil nil nil)
                       ("plot" "#+TBLNAME: ${1:tblname}\n#+PLOT: title:\"${2:$1}\" ind:1\n| ${3:X} | ${4:Y} |\n|---+---|\n| $0  |   |\n\n#+begin_src gnuplot :var data=\"$1\" :file \"$1.png\"\nreset\nset title \"$2\"\nset xlabel \"$3\"\nset ylabel \"$4\"\nunset key\nplot data u 1:2 w lines\n#+end_src\n" "gnuplot graph" nil nil nil nil nil nil)
                       ("#" "#+${1:$$(s-upcase yas/text)}: $0" "keyword" nil nil nil nil nil nil)
                       ("letter" "#+TITLE: ${1:Untitled Letter}\n#+LATEX_CLASS: koma-letter\n#+LCO: CBLetter CBAddress UScommercial9\n#+DATE: `(format-time-string \"%B %-d, %Y\")`\n\n#+TO_ADDRESS: ${2:Name}\\\\\\\\\n#+TO_ADDRESS: ${3:Street}\\\\\\\\\n#+TO_ADDRESS: ${4:Town}\n\n#+OPENING: ${5:To whom it may concern},\n\n$0\n\n#+CLOSING: Sincerely," "letter" nil nil nil nil nil nil)
                       ("presentation" "#+TITLE: ${1:Untitled Presentation}\n#+AUTHOR: `user-full-name`\n#+OPTIONS: toc:nil reveal_mathjax:t\n\n#+REVEAL_TRANS: `(cb-org:reveal-read-transition)`\n#+REVEAL_THEME: `(cb-org:reveal-read-theme)`\n\n$0" "presentation" nil nil nil nil nil nil)
                       ("el" "#+begin_src emacs-lisp\n$0\n#+end_src" "src elisp"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("py" "#+begin_src python\n$0\n#+end_src" "src python"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("rb" "#+begin_src ruby\n$0\n#+end_src" "src ruby"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sh" "#+begin_src sh\n$0\n#+end_src" "src sh"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sql" "#+begin_src sql\n$0\n#+end_src" "src sql"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sqlite" "#+header: :db ${1:name} :header :csv :nullvalue NULL\n#+begin_src sqlite\n$0\n#+end_src\n" "src sqlite"
                        (yas/bol\?)
                        nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
