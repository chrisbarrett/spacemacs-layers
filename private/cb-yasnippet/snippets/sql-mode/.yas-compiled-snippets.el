;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
                     '(("at" "ALTER TABLE ${1:table}\n$0" "alter table" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("c" "CASE ${1:?value}\n  $0\n  ELSE ${2:default}\nEND" "case" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("cast" "cast(${1:value} as ${2:type})" "cast" nil nil nil nil "direct-keybinding" nil)
                       ("ci" "CREATE INDEX ${1:name}_idx\nON ${2:table} (${3:col});" "create index" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("ct" "CREATE TABLE ${1:IF NOT EXISTS }${2:name} (\n    ${3:$2_id} INTEGER NOT NULL,\n    $0\n\n    CONSTRAINT PK_$2 PRIMARY KEY ($3)\n);" "create table" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil nil nil)
                       ("cv" "CREATE VIEW ${1:name}_vw (\n  ${2:attrs}\n)\nAS\nSELECT ${3:attrs}\nFROM $0;" "create view" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("cj" "CROSS JOIN ${1:rel}\n$0" "cross join" nil nil nil nil "direct-keybinding" nil)
                       ("d" "DISTINCT $0" "distinct" nil nil nil nil "direct-keybinding" nil)
                       ("di" "DROP INDEX ${1:name}_idx ON ${2:table};" "drop index" nil nil nil nil "direct-keybinding" nil)
                       ("fk" "CONSTRAINT FK_$1 FOREIGN KEY (${1:col})\n  REFERENCES ${2:f_table} (${3:f_col})" "foreign key constraint" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil nil nil)
                       ("g" "GROUP BY $0" "group by" nil nil nil nil "direct-keybinding" nil)
                       ("ine" "`(just-one-space)`IF NOT EXISTS`(just-one-space)`" "if not exists" nil nil nil nil "direct-keybinding" nil)
                       ("ij" "INNER JOIN ${1:rel}\n  $0" "inner join" nil nil nil nil "direct-keybinding" nil)
                       ("in" "INSERT INTO ${1:table} ($2)\nVALUES ($0)" "insert" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("loj" "LEFT OUTER JOIN ${1:rel}\n$0" "left outer join" nil nil nil nil "direct-keybinding" nil)
                       ("nj" "NATURAL JOIN ${1:rel}\n$0" "natural join" nil nil nil nil "direct-keybinding" nil)
                       ("nn" "NOT NULL" "not null" nil nil nil nil "direct-keybinding" nil)
                       ("o" "ON ${1:attr} = ${2:attr}\n$0" "on" nil nil nil nil "direct-keybinding" nil)
                       ("ob" "ORDER BY ${1:attrs}\n$0" "order by" nil nil nil nil "direct-keybinding" nil)
                       ("pk" "CONSTRAINT pk_$1 PRIMARY KEY (${1:col})" "primary key constraint" nil nil nil nil "direct-keybinding" nil)
                       ("roj" "RIGHT OUTER JOIN ${1:rel}\n$0" "right outer join" nil nil nil nil "direct-keybinding" nil)
                       ("rb" "ROLLBACK $0" "rollback" nil nil nil nil "direct-keybinding" nil)
                       ("rbt" "ROLLBACK TO SAVEPOINT ${1:name};\n$0" "rollback to savepoint" nil nil nil nil "direct-keybinding" nil)
                       ("sp" "SAVEPOINT ${1:name};\n$0" "savepoint" nil nil nil nil "direct-keybinding" nil)
                       ("s" "SELECT ${1:attrs}\nFROM $0" "select" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("st" "START TRANSACTION;\n\n$0\n\nCOMMIT;" "start transaction...commit" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("ua" "UNION ALL\n$0" "union all" nil nil nil nil "direct-keybinding" nil)
                       ("uc" "CONSTRAINT ${2:name}_ak UNIQUE (${3:cols})" "uniqueness constraint" nil nil nil nil "direct-keybinding" nil)
                       ("up" "UPDATE ${1:table}\nSET ${2:row} = $0" "update" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 't))
                        nil "direct-keybinding" nil)
                       ("u" "USING (${1:attr})\n$0" "using" nil nil nil nil "direct-keybinding" nil)
                       ("wh" "WHEN ${1:test} THEN $0" "when...then" nil nil nil nil "direct-keybinding" nil)
                       ("w" "WHERE $0" "where" nil nil nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
