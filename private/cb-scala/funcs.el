;;; Smart ops

(defun scala/equals ()
  (interactive)
  (super-smart-ops-insert "="))

(defun scala/colon ()
  (interactive)
  (core/insert-smart-op-no-leading-space ":"))

(defmacro define-scala-variance-op-command (sym op)
  "Define command named SYM to insert a variance operator OP."
  `(defun ,sym ()
     "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
     (interactive "*")
     (cond
      ;; No padding at the start of type parameter.
      ((thing-at-point-looking-at (rx "[" (* space)))
       (delete-horizontal-space)
       (insert ,op))
      ;; Leading padding after a comma, e.g. for a type parameter or function call.
      ((thing-at-point-looking-at (rx "," (* space)))
       (just-one-space)
       (insert ,op))
      ;; Otherwise leading and trailing padding.
      (t
       (super-smart-ops-insert ,op)))))

(define-scala-variance-op-command scala/plus "+")
(define-scala-variance-op-command scala/minus "-")


;;; Interactive

(defun scala/join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))
