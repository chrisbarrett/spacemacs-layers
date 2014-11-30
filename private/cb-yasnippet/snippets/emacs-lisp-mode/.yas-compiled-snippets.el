;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("-" "(->> $0)" "->>" nil nil nil nil nil nil)
                       ("pr" ";; Package-Requires: ((s \"1.9.0\") (f \"0.16.0\") (dash \"2.5.0\") (cl-lib \"0.3\"))" "Package-Requires"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("ah" "(add-hook '$0)" "add-hook" nil nil nil nil nil nil)
                       ("atl" "(add-to-list '$0)" "add-to-list" nil nil nil nil nil nil)
                       ("a" "(after '$0)" "after" nil nil nil nil nil nil)
                       ("as" "(cl-assert $0)" "assert" nil nil nil nil nil nil)
                       ("al" "(autoload '${1:ident} \"${2:file}\")" "autoload"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("bs" "(buffer-substring ${1:beg} ${2:end})$0" "buffer-substring" nil nil nil nil nil nil)
                       ("lo" "(cl-loop${1: for ${2:x} in }$0)" "cl-loop" nil nil nil nil nil nil)
                       ("cs" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" "comment separator"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("c" "(cond\n  (($1)\n   $0)\n\n  (t\n    ))" "cond" nil nil nil nil nil nil)
                       ("cc" "(condition-case ${1:err}\n    $0\n  (error $2))" "condition-case" nil nil nil nil nil nil)
                       ("csf" "(custom-set-faces\n  '($0))" "custom-set-faces"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("csv" "(custom-set-variables\n  '($0))" "custom-set-variables"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("da" "(defadvice $1 (${2:around} ${3:desc} ${4:activate})\n  ${5:\"$6\"}\n  $0)" "defadvice" nil nil nil nil nil nil)
                       ("k" "(defconst `(yas/find-identifier-prefix)`$0)\n" "defconst"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dc" "(defcustom ${1:name} ${2:value}\n  \"$3\"\n  :group '`(yas/find-group-for-snippet)`\n  :type '$4)" "defcustom"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("df" "(defface $1\n  '((t\n     ($0)))\n  \"Face for $3\"\n  :group '${4:`(yas/find-group-for-snippet)`})\n" "defface"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("g" "(defgroup `(file-name-sans-extension (file-name-nondirectory buffer-file-name))` nil\n  \"$0\"\n  :group '${1:languages}\n  :prefix \"`(file-name-sans-extension (file-name-nondirectory buffer-file-name))`-\")\n" "defgroup"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("dk" "(define-key ${1:keymap} (kbd \"${2:key}\") '${3:cmd})" "define-key" nil nil nil nil nil nil)
                       ("dm" "(${3:$(yas/defmacro-form-for-arglist yas/text)} ${1:`(yas/find-identifier-prefix)`$2} ($3)\n  ${4:\"$5${6:${3:$(yas/process-docstring yas/text)}}\"\n  }$0)\n" "defmacro"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("ds" "(cl-defstruct (${1:Name}\n                 (:constructor ${2:$1} (${3:arglist})))\n    \"${4:Description}${3:$(unless (equal \"slots\" yas/text) (yas/process-docstring yas/text))}\"\n    ${3:$(s-join \" \" (-map 'symbol-name (yas/simplify-arglist yas/text)))})\n" "defstruct"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("d" "(${3:$(yas/defun-form-for-arglist yas/text)} ${1:`(yas/find-identifier-prefix)`$2} ($3)\n  ${4:\"$5${6:${3:$(yas/process-docstring yas/text)}}\"\n  }$0)" "defun"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("v" "(defvar `(yas/find-identifier-prefix)`$0)\n" "defvar"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("b" "(cl-destructuring-bind ($1) $2\n  $0)\n" "destructuring-bind" nil nil nil nil nil nil)
                       ("hdr" ";;; `(file-name-nondirectory buffer-file-name)` --- $1\n\n;; Copyright (C) `(format-time-string \"%Y\")` `user-full-name`\n\n;; Author: `user-full-name` <`user-mail-address`>\n;; Version: `0.1`\n\n;; This file is not part of GNU Emacs.\n\n;; This program is free software: you can redistribute it and/or modify\n;; it under the terms of the GNU General Public License as published by\n;; the Free Software Foundation, either version 3 of the License, or\n;; (at your option) any later version.\n\n;; This program is distributed in the hope that it will be useful,\n;; but WITHOUT ANY WARRANTY; without even the implied warranty of\n;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n;; GNU General Public License for more details.\n\n;; You should have received a copy of the GNU General Public License\n;; along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\n;;; Commentary:\n\n;; $1\n\n;;; Code:\n\n$0" "elisp file header" nil nil nil nil nil nil)
                       ("ert" "(ert-deftest ${1:test-name} ()\n  (should (equal $0)))" "ert-deftest"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("f" "(format \"${1:%s}\" $0)" "format" nil nil nil nil nil nil)
                       ("gc" "(goto-char ${1:(point-min)})$0" "goto-char" nil nil nil nil nil nil)
                       ("i" "(if ($0))" "if" nil nil nil nil nil nil)
                       ("il" "(-if-let ($0)\n)" "if-let" nil nil nil nil nil nil)
                       ("ie" "(ignore-errors $0)" "ignore-errors" nil nil nil nil nil nil)
                       ("in" "(interactive$1)$0" "interactive" nil nil nil nil nil nil)
                       ("\\" "(lambda ($1) $0)" "lambda" nil nil nil nil nil nil)
                       ("l" "(let (($0)))" "let"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("lbp" "(line-beginning-position)" "line-beginning-position" nil nil nil nil nil nil)
                       ("lep" "(line-end-position)" "line-end-position" nil nil nil nil nil nil)
                       ("ls" "(list $0)" "list" nil nil nil nil nil nil)
                       ("lsk" "(local-set-key (kbd \"$1\") '$0)" "local-set-key" nil nil nil nil nil nil)
                       ("m" "(message \"$0\")" "message" nil nil nil nil nil nil)
                       ("nl" "(newline)" "newline" nil nil nil nil nil nil)
                       ("pt" "(point)" "point" nil nil nil nil nil nil)
                       ("pM" "(point-max)" "point-max" nil nil nil nil nil nil)
                       ("pm" "(point-min)" "point-min" nil nil nil nil nil nil)
                       ("p" "(progn\n $0)" "progn" nil nil nil nil nil nil)
                       ("prov" "(provide '`(file-name-sans-extension (file-name-nondirectory buffer-file-name))`)" "provide" nil nil nil nil nil nil)
                       ("r" "(require '$0)" "require"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("se" "(save-excursion\n  $0)" "save-excursion" nil nil nil nil nil nil)
                       ("sr" "(save-restriction\n  (narrow-to-region $1)\n  $0)" "save-restriction" nil nil nil nil nil nil)
                       ("sb" "(search-backward $0)" "search-backward" nil nil nil nil nil nil)
                       ("sbr" "(search-backward-regexp (rx $0))" "search-backward-regexp" nil nil nil nil nil nil)
                       ("sf" "(search-forward $0)" "search-forward" nil nil nil nil nil nil)
                       ("sfr" "(search-forward-regexp (rx $0))" "search-forward-regexp" nil nil nil nil nil nil)
                       ("s" "(setq $0)" "setq"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("sl" "(setq-local ${1:var} ${2:value})$0" "setq-local" nil nil nil nil "direct-keybinding" nil)
                       ("u" "(unless ($0))" "unless" nil nil nil nil nil nil)
                       ("up" ";; `${1:$(format \"%s\" yas/text)}' $2\n(use-package $1\n  $0)" "use-package"
                        (yas/bol\?)
                        nil nil nil nil nil)
                       ("w" "(when ($0))" "when" nil nil nil nil nil nil)
                       ("wl" "(-when-let ($0)\n)" "when-let" nil nil nil nil nil nil)
                       ("wh" "(while ($0))" "while" nil nil nil nil nil nil)
                       ("wcb" "(with-current-buffer $0)\n" "with-current-buffer" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
