;;; funcs.el --- cb-cpp Funcs File for Spacemacs
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-insert-state "evil-states")
(autoload 'yas-expand-snippet "yasnippet")

(defun cb-cpp/M-RET ()
  (interactive "*")
  (cond
   ((s-matches? (rx bol (* space) "using" space) (cb-buffers-current-line))
    (cb-cpp/semicolon-then-newline)
    (yas-expand-snippet "using $0;")
    (message "Inserted using directive."))

   ((s-matches? (rx bol (* space) "#include" (+ space) "<") (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-expand-snippet "#include <$0>")
    (message "Inserted library include."))

   ((s-matches? (rx bol (* space) "#include" (+ space) "\"") (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-expand-snippet "#include \"$0\"")
    (message "Inserted header include."))

   (t
    (cb-cpp/semicolon-then-newline)))

  (evil-insert-state))

(defun cb-cpp/semicolon-then-newline ()
  (goto-char (line-end-position))
  (delete-horizontal-space)
  (unless (equal ?\; (char-before))
    (insert ";"))
  (newline-and-indent))

(defun cb-cpp/C-RET ()
  (interactive "*")
  (goto-char (line-end-position))
  (delete-horizontal-space)
  (unless (equal ?\; (char-before))
    (insert ";"))
  (evil-insert-state))

(defun cb-cpp/resolve (qualified-name)
  "Return QUALIFIED-NAME for use in snippets.

If that identifier's namespace has been imported, use the name
without qualification."
  (-let [(ns unqualified) (s-split "::" qualified-name)]
    (if (s-matches? (rx-to-string `(or (and bol (* space)
                                            "using" (+ space) "namespace" (+ space) ,ns (* space) ";")
                                       (and "using" (+ space) ,qualified-name (* space) ";")))
                    (buffer-string))
        unqualified
      qualified-name)))

;;; funcs.el ends here
