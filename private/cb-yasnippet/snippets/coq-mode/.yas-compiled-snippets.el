;;; Compiled snippets and support files for `coq-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'coq-mode
                     '(("c" "Case \"$1\". $0" "Case"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("cdef" "Require String. Open Scope string_scope.\n\nLtac move_to_top x :=\n  match reverse goal with\n    | H : _ |- _ => try move x after H\n  end.\n\nTactic Notation \"assert_eq\" ident(x) constr(v) :=\n  let H := fresh in\n  assert (x = v) as H by reflexivity;\n    clear H.\n\nTactic Notation \"Case_aux\" ident(x) constr(name) :=\n  first\n    [ set (x := name); move_to_top x\n    | assert_eq x name; move_to_top x\n    | fail 1 \"because we are working on a different case\"\n    ].\n\nTactic Notation \"Case\" constr(name) := Case_aux Case name.\nTactic Notation \"SCase\" constr(name) := Case_aux SCase name.\nTactic Notation \"SSCase\" constr(name) := Case_aux SSCase name.\nTactic Notation \"SSSCase\" constr(name) := Case_aux SSSCase name.\nTactic Notation \"SSSSCase\" constr(name) := Case_aux SSSSCase name.\nTactic Notation \"SSSSSCase\" constr(name) := Case_aux SSSSSCase name.\nTactic Notation \"SSSSSSCase\" constr(name) := Case_aux SSSSSSCase name.\nTactic Notation \"SSSSSSSCase\" constr(name) := Case_aux SSSSSSSCase name.\n" "Case Definition"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("ch" "Check $0." "Check"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("cof" "CoFixpoint ${1:name and params} : ${3:T} :=\n  $0." "CoFixpoint"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil "direct-keybinding" nil)
                       ("coi" "CoInductive ${1:name} := $0." "CoInductive"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("d" "Definition ${1:name and params} : ${3:T} :=\n  $0." "Definition"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("e" "Eval ${1:compute} in $0." "Eval"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ex" "Example ${1:name} : $0.\nProof. reflexivity. Qed." "Example"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("f" "Fixpoint ${1:name and params} : ${3:T} :=\n  $0." "Fixpoint"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("hr" "Hint Rewrite $0." "Hint Rewrite"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("hy" "Hypothesis ${1:ident} : ${2:T}." "Hypothesis"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("in" "Inductive ${1:name} := $0." "Inductive"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("lm" "Lemma ${1:name} :\n  forall $0.\nProof.\n  intros.\n  reflexivity.\nQed." "Lemma"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("mod" "Module ${1:Name}.\n\n  $0\n\nEnd $1.\n" "Module"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("p" "Proof.\n  $0\n  reflexivity.\nQed." "Proof"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("q" "Qed." "Qed"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("rm" "Remark ${1:name} :\n  forall $0.\nProof.\n  intros.\n  reflexivity.\nQed." "Remark"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("r" "Require $0." "Require"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("re" "Require Export ${1:modids}." "Require Export"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ri" "Require Import ${1:modids}." "Require Import"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("sc" "SCase \"$1\". $0" "SCase"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("ssc" "SSCase \"$1\". $0" "SSCase"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("sa" "SearchAbout ${1:query}. $0" "SearchAbout" nil nil nil nil nil nil)
                       ("sec" "Section ${1:name}.\n  $0\n\nEnd $1." "Section"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil "direct-keybinding" nil)
                       ("th" "Theorem ${1:name} :\n  forall $0.\nProof.\n  intros.\n  reflexivity.\nQed." "Theorem"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("v" "Variable ${1:ident} : ${2:T}." "Variable"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("ap" "apply ${1:hyp}. $0" "apply" nil nil nil nil nil nil)
                       ("aw" "apply ${1:hyp} with ${3:val}. $0" "apply...with" nil nil nil nil nil nil)
                       ("a" "`(just-one-space)`as [| ${1:x'}]" "as [| x']" nil nil nil nil nil nil)
                       ("as" "assert (${1:H} : $2).\n+ $0\n+ apply $1.\n" "assert"
                        (yas/bol\?)
                        nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("ass" "assumption. $0" "assumption" nil nil nil nil nil nil)
                       ("ds" "destruct ${1:x}. $0" "destruct" nil nil nil nil nil nil)
                       ("fun" "fun ${1:bindings} => $0" "fun" nil nil nil nil nil nil)
                       ("gd" "generalize dependent ${1:x}. $0" "generalize dependent" nil nil nil nil nil nil)
                       ("i" "if ${1:test}\nthen ${2:expr}\nelse ${3:expr}" "if" nil nil nil nil nil nil)
                       ("ind" "induction ${1:n}. $0" "induction" nil nil nil nil nil nil)
                       ("int" "intros $1. $0" "intros" nil nil nil nil "direct-keybinding" nil)
                       ("inv" "inversion ${1:hyp}. $0" "inversion" nil nil nil nil nil nil)
                       ("l" "let ${1:binding} := ${2:expr} in $0" "let...in"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("m" "match ${1:expr} with\n| ${2:case} => $0\nend" "match" nil nil nil nil "direct-keybinding" nil)
                       ("|" "| ${1:case} => $0" "match-case" nil nil nil nil nil nil)
                       ("not" "Notation \"${1:syntax}\" := (${2:def})${3: (at level ${4:n})}." "notation"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("rf" "reflexivity." "reflexivity" nil nil nil nil nil nil)
                       ("rp" "replace (${1:t}) with (${2:u}). $0" "replace" nil nil nil nil nil nil)
                       ("rw" "rewrite ${1:H}. $0" "rewrite" nil nil nil nil nil nil)
                       ("si" "simpl. $0" "simpl" nil nil nil nil nil nil)
                       ("sy" "symmetry. $0" "symmetry" nil nil nil nil nil nil)
                       ("uf" "unfold $0." "unfold" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
