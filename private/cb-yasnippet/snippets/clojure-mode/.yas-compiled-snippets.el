;;; Compiled snippets and support files for `clojure-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojure-mode
                     '(("-" "(->> $0)" "->>" nil nil nil nil nil nil)
                       ("?" "(-?>> $0)" "-?>>" nil nil nil nil nil nil)
                       ("F" "FREE" "FREE" nil nil nil nil nil nil)
                       ("case" "(case ${1:expr}\n  ${2:value} ${3:expr}\n  $0)" "case" nil nil nil nil nil nil)
                       ("c" "(cond\n  ${1:test} ${2:expr}${3:\n  :else ${4:expr}})" "cond" nil nil nil nil nil nil)
                       ("v" "(def ${1:name} ${2:value})" "def"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("di" "(definst ${1:name} ${2:\n  \"$3\"\n  }[${4:${5:freq ${6:440}}${7: att ${8:0.01}}${9: sus ${10:0.4}}${11: rel ${12:0.1}}${13: amp ${14:0.4}}}]\n  $0)" "definst"
                        (and
                         (yas/bol\?)
                         (true\? overtone-mode))
                        nil nil nil nil nil)
                       ("m" "(defmacro ${1:name} ${2:\n  \"$3\"\n  }[$4]\n  $0)" "defmacro"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dm" "(defmethod ${1:name} ${2:dispatch-value} ${4:${5:instance-name} }${6:impl...})" "defmethod"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dmul" "(defmulti ${1:name}\n  \"$2\"\n  (fn [${$3:x}] ${4:impl...}))" "defmulti"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("d" "(defn ${1:name} ${2:\n  \"$3\"\n  }[$4]\n  $0)" "defn"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dp" "(defprotocol ${1:Name}\n  \"$2\"\n  (${3:fname} [this${4:$$(cbclj:pad-for-arglist yas/text)}] \"$5\"))" "defprotocol"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dr" "(defrecord ${1:Name} [${2:fields...}])" "defrecord"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ds" "(defsynth ${1:name} ${2:\n  \"$3\"\n  }[${4:freq ${5:440}}$6]\n  $0)" "defsynth"
                        (and
                         (yas/bol\?)
                         (true\? overtone-mode))
                        nil nil nil nil nil)
                       ("dt" "(deftype ${1:Name} [${2:fields...}])" "deftype"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("eg" "(env-gen ${1:(env-lin ${2:att sus rel})}$3 :action FREE)" "env-gen"
                        (true\? overtone-mode)
                        nil nil nil nil nil)
                       ("el" "(env-lin ${1:att} ${2:sus} ${3:rel})" "env-lin"
                        (true\? overtone-mode)
                        nil nil nil nil nil)
                       ("ep" "(extend-protocol ${1:Protocol}\n  ${2:Type}\n  (${3:method} [this${4:$$(cbclj:pad-for-arglist yas/text)}] ${5:impl...}))" "extend-protocol"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("et" "(extend-type ${1:Name}\n  ${2:Protocol}\n  (${3:method} [this${4:$$(cbclj:pad-for-arglist yas/text)}] ${5:impl...}))" "extend-type"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("for" "(for [${1:binding} ${2:expr}] $0)" "for" nil nil nil nil nil nil)
                       ("i" "(if $0)" "if" nil nil nil nil nil nil)
                       ("in" "(if-not [${1:binding} ${2:value}]\n  $0)" "if-not" nil nil nil nil nil nil)
                       ("l" "(let [${1:binding} ${2:value}]\n  $0)" "let" nil nil nil nil nil nil)
                       ("ns" "(ns ${1:`(cbclj:ns-for-current-buf)`}\n  \"$2\"${3:\n  (:use [${4:ns}${5: :only [${6:syms...}]}]$7)}${8:\n  (:require [${9:ns}${10: :as ${11:alias}}]$12)}${13:\n  (:import ${14:symbols/lists...})})" "ns"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("p" "(println \"$1\")" "println" nil nil nil nil nil nil)
                       ("r" "(reify ${1:Class}\n  (${2:method} [this${3:$$(cbclj:pad-for-arglist yas/text)}]\n    ${4:impl...}))" "reify" nil nil nil nil nil nil)
                       ("w" "(when $0)" "when" nil nil nil nil nil nil)
                       ("wl" "(when-let [${1:binding} ${2:value}]\n  $0)" "when-let" nil nil nil nil nil nil)
                       ("u" "(when-not $0)" "when-not" nil nil nil nil nil nil)
                       ("wo" "(with-open [${1:r} ${2:(java.io.FileReader. ${3:path})}]\n  $0)" "with-open" nil nil nil nil nil nil)
                       ("wos" "(with-out-str\n  $0)" "with-out-str" nil nil nil nil nil nil)
                       ("z" "(zero? $0)" "zero?" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
