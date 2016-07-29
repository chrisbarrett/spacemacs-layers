(require 'let-alist)
(require 's)
(require 'dash)

(defvar sql/last-type-search-query nil)

(with-eval-after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'sql-analysis-mode))

(defvar sql-analysis-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'quit-window)
    keymap))

(evil-define-key 'normal sql-analysis-mode-map
  (kbd "q") 'quit-window
  [down-mouse-1] 'push-button)

(define-derived-mode sql-analysis-mode nil "SQL Analysis"
  "Mode for displaying SQL buffer analysis results.")

(defun sql-find-attrs-with-type (type-names)
  (interactive (list (let* ((prompt (if sql/last-type-search-query
                                        (format "Search by type name (default: %s): "
                                                (s-join "|" sql/last-type-search-query))
                                      "Search by type name: "))
                            (input (s-split (rx (or space "|"))
                                            (read-string prompt nil nil sql/last-type-search-query))))
                       (setq sql/last-type-search-query input)
                       input)))

  (let ((matches (sql/search-for-attrs-with-type-in-buffer type-names (current-buffer))))
    (sql/display-search-results (s-join "|" type-names) matches)))

(defun sql/search-for-attrs-with-type-in-buffer (type-names buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((types-rx (sql/make-rx-matching-attrs-with-types type-names))
            (acc))
        (while (and (not (eobp))
                    (search-forward-regexp types-rx nil t))
          (let ((cur `((line . ,(line-number-at-pos))
                       (pos . ,(line-beginning-position))
                       (text . ,(substring-no-properties (match-string 1)))
                       (table . ,(sql/find-table-for-create-table-expr-at-point buffer))
                       (buffer . ,buffer)
                       (type . ,(substring-no-properties (match-string 1))))))
            (setq acc (cons cur acc))))

        (nreverse acc)))))

(defun sql/make-rx-matching-attrs-with-types (type-names)
  (rx-to-string `(and bol (* space)
                      (? "`") (group-n 1 (+ (not (any space "`")))) (? "`")
                      (+ space)
                      (group-n 2 (regex ,(regexp-opt type-names))))
                t))

(defun sql/find-table-for-create-table-expr-at-point (buf)
  (save-excursion
    (when (search-backward-regexp (rx bol (* space)
                                      "create" (+ space) "table" (+ space)
                                      (? "`")
                                      (group (+ (not (any "(" "`" space))))
                                      (? "`"))
                                  nil t)

      `((name . ,(substring-no-properties (match-string 1)))
        (line . ,(line-number-at-pos))
        (pos . ,(match-beginning 1))
        (buffer . ,buf)))))

(defun sql/display-search-results (query results)
  (let ((buf (get-buffer-create "*SQL buffer analysis*"))
        (grouped (->> results
                      (--group-by (let-alist it .table.name))
                      (--sort (string-lessp (s-downcase (car it)) (s-downcase (car other)))))))

    (pop-to-buffer buf)
    (erase-buffer)
    (sql-analysis-mode)

    (insert (format "Searching for '%s': %s result(s)\n\n"
                    query
                    (length results)))

    (-each grouped
      (-lambda ((_ . matches))
        (let ((table (cdr (assoc 'table (car matches)))))
          (sql/insert-table-heading-button table))
        (newline)
        (--each matches (sql/insert-search-result-button it))
        (newline)))

    (goto-char (point-min))))

(defun sql/insert-table-heading-button (table)
  (let-alist table
    (insert-text-button
     .name
     'face (list :foreground cb-vars-solarized-hl-blue)
     'action 'sql/button-ret
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to table declaration in another window")))

(defun sql/insert-search-result-button (result)
  (let-alist result

    (insert-text-button
     (format "%s:" .line)
     'action 'sql/button-ret
     'face (list :foreground "lightgray")
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to attribute in another window")

    (insert-text-button
     (concat " " .text)
     'action 'sql/button-ret
     'face nil
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to attribute in another window")

    (newline)))

(defun sql/button-ret (button)
  (interactive (list (text-properties-at (point))))
  (let ((pt (button-get button 'position))
        (buf (button-get button 'buffer)))
    (with-current-buffer (pop-to-buffer buf)
      (goto-char pt))))
