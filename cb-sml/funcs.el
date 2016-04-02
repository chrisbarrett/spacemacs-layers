;;; funcs.el --- Supporting functions for SML config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'comint-send-input "comint")
(autoload 'evil-insert-state "evil-states")
(autoload 'yas-expand-snippet "yasnippet")

(defun cb-sml/at-datatype-decl? ()
  (or (s-matches? (rx bol (* space) (or "|" "datatype")) (cb-buffers-current-line))
      (when (s-matches? "=" (cb-buffers-current-line))
        (save-excursion
          (forward-line -1)
          (s-matches? (rx bol (* space) "datatype") (cb-buffers-current-line))))))

(defun cb-sml/at-case-header? ()
  (s-matches? (rx "case" (+ nonl) "of" (* space) eol) (cb-buffers-current-line)))

(defun cb-sml/at-case? ()
  (s-matches? (rx bol (* space) (or "of" "|") (+ nonl) "=>") (cb-buffers-current-line)))

(defun cb-sml/at-val-binding? ()
  (s-matches? (rx bol (* space) "val" eow) (cb-buffers-current-line)))

(defun cb-sml/at-fun-case? ()
  (-when-let (fun-name (cb-sml/maybe-fname-for-case-at-point))
    (s-matches? (rx-to-string `(and bol (* space) "|" (+ space) ,fun-name))
                (cb-buffers-current-line))))

(defun cb-sml/at-fun-decl? ()
  (s-matches? (rx bol (* space) (or "and" "fun") eow) (cb-buffers-current-line)))

(defun cb-sml/maybe-fname-for-case-at-point ()
  (save-excursion
    (goto-char (line-end-position))
    (when (search-backward-regexp (rx bol (* space) (or "and" "fun") (+ space)
                                      (group (+ (not space))))
                                  nil t)
      (match-string 1))))

(defun cb-sml/m-ret ()
  (interactive)
  (cond
   ((cb-sml/at-fun-case?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to col)
      (yas-expand-snippet (format "| %s ${1:bindings} = $0" (cb-sml/maybe-fname-for-case-at-point)))
      (message "Inserted function case.")))

   ((cb-sml/at-fun-decl?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to (+ col sml-indent-level))
      (yas-expand-snippet (format "| %s ${1:bindings} = $0" (cb-sml/maybe-fname-for-case-at-point)))
      (message "Inserted function case.")))

   ((cb-sml/at-case-header?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column (+ col sml-indent-level))
      (yas-expand-snippet "| ${1:binding} => $0")
      (message "Inserted pattern match case.")))

   ((cb-sml/at-case?)
    (let ((col (if (s-matches? (rx bol (* space) "of" eow) (cb-buffers-current-line))
                   (1+ (current-indentation))
                 (current-indentation))))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "| ${1:binding} => $0")
      (message "Inserted pattern match case.")))

   ((cb-sml/at-datatype-decl?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (insert "| ")
      (message "Inserted case.")))

   ((and (cb-sml/at-val-binding?)
         (equal "sig" (f-ext (buffer-file-name))))
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "val ${1:name} : ${2:type}")
      (message "Inserted val declaration.")))

   ((cb-sml/at-val-binding?)
    (let ((col (current-indentation)))
      (goto-char (line-end-position))
      (newline)
      (indent-to-column col)
      (yas-expand-snippet "val ${1:name} = $0")
      (message "Inserted val declaration.")))

   (t
    (reindent-then-newline-and-indent)))

  (evil-insert-state))

(defun cb-sml/inf-sml-m-ret ()
  (interactive)
  (goto-char (point-max))
  (insert ";")
  (comint-send-input)
  (evil-insert-state))

(defun cb-sml/ret ()
  (interactive)
  (let ((col (current-indentation)))
    (sp-generic-prog-ret)
    (when (s-blank? (s-trim (cb-buffers-current-line)))
      (delete-horizontal-space)
      (indent-to col))))

;; snippet utils

(defun cb-sml/at-start-of-expr-for-snippet? (snippet-trigger-length)
  (let ((line-to-trigger (buffer-substring (line-beginning-position) (- (point) snippet-trigger-length))))
    (or (yas/bol?)
        (s-matches? (rx (or "=" "=>") (* space) eos) line-to-trigger))))

;;; funcs.el ends here
