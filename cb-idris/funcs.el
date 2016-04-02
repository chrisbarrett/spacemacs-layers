;;; funcs.el --- Funcs for cb-idris layer. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'thingatpt)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'cb-buffers-filtera "cb-buffers")
(autoload 'evil-insert-state "evil-states")
(autoload 'idris-newline-and-indent "idris-commands")

(defun idris/after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "[|") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun idris/before-subexp-closing? ()
  (s-matches? (rx bol (* space) (or "}" "]" "-}" "|]"))
              (buffer-substring (point) (line-end-position))))

(defun idris/smart-space ()
  "Insert a space, with context-sensitive padding."
  (interactive)
  (cond
   ((and (idris/after-subexpr-opening?) (idris/before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (sp-generic-prog-space))))


;;; Commands

(defun idris/switch-to-src ()
  "Pop to the last idris source buffer."
  (interactive)
  (-if-let ((buf) (cb-buffers-filtera (derived-mode-p 'idris-mode)))
      (pop-to-buffer buf)
    (error "No idris buffers")))

(defun idris/backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (cond
   ((and (idris/after-subexpr-opening?)
         (idris/before-subexp-closing?)
         (thing-at-point-looking-at (rx (+ space))))
    (delete-horizontal-space))

   ((and (s-matches? (rx "{-" eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol "-}")
                     (buffer-substring (point) (line-end-position))))
    (delete-char 1)
    (delete-char -1))

   (t
    (sp-generic-prog-backspace))))

;;; Smart M-RET

(defun idris/data-start-pos ()
  "Find the start position of the datatype declaration at point."
  (save-excursion
    (end-of-line)
    (when (search-backward-regexp (rx bol (* space) (or "record" "data") eow) nil t)
      (skip-chars-forward " \t")
      (point))))

(defun idris/data-end-pos ()
  "Find the end position of the datatype declaration at point."
  (save-excursion
    (let ((start (point)))

      (goto-char (idris/data-start-pos))
      (forward-line)
      (goto-char (line-beginning-position))

      (let ((end
             (when (search-forward-regexp
                    (rx bol (or (and (* space) eol) (not (any space "|"))))
                    nil t)
               (forward-line -1)
               (line-end-position))))
        (if (and end (<= start end))
            end
          (point-max))))))

(cl-defun idris/data-decl-at-pt ()
  "Return the data declaration at point."
  (-when-let* ((start (idris/data-start-pos))
               (end (idris/data-end-pos)))
    (buffer-substring-no-properties start end)))

(defun idris/at-data-decl? ()
  (-when-let (dd (idris/data-decl-at-pt))
    (let ((lines (s-split "\n" dd)))
      (or (equal 1 (length lines))
          (->> (-drop 1 lines)
               (--all? (s-matches? (rx bol (or space "|")) it)))))))

(defun idris/function-name-at-pt ()
  "Return the name of the function at point."
  (save-excursion
    (back-to-indentation)
    (-when-let (s (thing-at-point 'symbol))
      (when (s-matches? (rx-to-string `(and ,s (* space) ":"))
                        (buffer-string))
        s))))

(defun idris/ret ()
  "Indent and align on newline."
  (interactive "*")
  (if (s-matches? comment-start (cb-buffers-current-line))
      (comment-indent-new-line)

    (cond

     ((s-matches? (rx space "->" (* space))
                  (buffer-substring (line-beginning-position) (point)))
      (newline)
      (delete-horizontal-space)
      (indent-for-tab-command))

     ((s-matches? (rx bol (* space) eol) (cb-buffers-current-line))
      (delete-horizontal-space)
      (newline))

     (t
      (sp-generic-prog-ret)))))

(defun idris/meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
- At functions, create a new case for the function.
- At types, add a 'where' statement if one does not exist.
- At comments, fill paragraph and insert a newline."
  (interactive)
  (cond

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (* space) "|") (cb-buffers-current-line))
    (let ((col (save-excursion (back-to-indentation) (current-column))))
      (goto-char (line-end-position))
      (newline)
      (indent-to col))

    (insert "| ")
    (message "New data case"))

   ;; Insert new type decl case below the current one.
   ((and (s-matches? (rx bol (* space) "data") (cb-buffers-current-line))
         (not (s-matches? "where" (cb-buffers-current-line))))

    (-if-let (col (save-excursion
                    (goto-char (line-beginning-position))
                    (search-forward "=" nil t)
                    (current-column)))
        (progn
          (goto-char (line-end-position))
          (newline)
          (indent-to (1- col)))

      (goto-char (line-end-position))
      (idris-newline-and-indent))

    (insert "| ")
    (message "New data case"))

   ;; Create new function case.
   ((idris/function-name-at-pt)
    (goto-char (line-end-position))
    (let ((fn (idris/function-name-at-pt))
          (col (save-excursion
                 (back-to-indentation)
                 (current-column))))

      (unless (s-matches? (rx bol (* space) eol) (cb-buffers-current-line))
        (newline))

      (indent-to-column col)
      (insert fn)
      (just-one-space)))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (* space) ",") (cb-buffers-current-line))
    (idris/newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Create a new line in a comment.
   ((s-matches? comment-start (cb-buffers-current-line))
    (fill-paragraph)
    (comment-indent-new-line)
    (message "New comment line"))

   (t
    (goto-char (line-end-position))
    (idris/ret)))

  (evil-insert-state))

(defun idris/newline-indent-to-same-col ()
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

;;; funcs.el ends here
