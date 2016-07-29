;;; cb-sql-find-attrs-with-type.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'let-alist)
(require 's)
(require 'dash)
(require 'cb-vars)

(defvar sql/last-type-search-query nil)

(defvar cb-sql-find-attrs-with-type-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'quit-window)
    keymap))

(define-derived-mode cb-sql-find-attrs-with-type-mode nil "SQL Analysis"
  "Mode for displaying SQL buffer analysis results.")

(defun cb-sql-find-attrs-with-type--types-regexp (type-names)
  (rx-to-string `(and bol (* space)
                      (? "`") (group-n 1 (+ (not (any space "`")))) (? "`")
                      (+ space)
                      (group-n 2 (regex ,(regexp-opt type-names))))
                t))

(defun cb-sql-find-attrs-with-type--search-in-buffer (type-names buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((types-rx (cb-sql-find-attrs-with-type--types-regexp type-names))
            (acc))
        (while (and (not (eobp))
                    (search-forward-regexp types-rx nil t))
          (let ((cur `((line . ,(line-number-at-pos))
                       (pos . ,(line-beginning-position))
                       (text . ,(substring-no-properties (match-string 1)))
                       (table . ,(cb-sql-find-attrs-with-type--find-create-table-for-pt buffer))
                       (buffer . ,buffer)
                       (type . ,(substring-no-properties (match-string 1))))))
            (setq acc (cons cur acc))))

        (nreverse acc)))))

(defun cb-sql-find-attrs-with-type--find-create-table-for-pt (buf)
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

(defun cb-sql-find-attrs-with-type--display-results (query results)
  (let ((buf (get-buffer-create "*SQL buffer analysis*"))
        (grouped (->> results
                      (--group-by (let-alist it .table.name))
                      (--sort (string-lessp (s-downcase (car it)) (s-downcase (car other)))))))

    (pop-to-buffer buf)
    (erase-buffer)
    (cb-sql-find-attrs-with-type-mode)

    (insert (format "Searching for '%s': %s result(s)\n\n"
                    query
                    (length results)))

    (-each grouped
      (-lambda ((_ . matches))
        (let ((table (cdr (assoc 'table (car matches)))))
          (cb-sql-find-attrs-with-type--insert-table-heading-button table))
        (newline)
        (--each matches (cb-sql-find-attrs-with-type--insert-search-result-button it))
        (newline)))

    (goto-char (point-min))))

(defun cb-sql-find-attrs-with-type--insert-table-heading-button (table)
  (let-alist table
    (insert-text-button
     .name
     'face (list :foreground cb-vars-solarized-hl-blue)
     'action 'cb-sql-find-attrs-with-type--select-button
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to table declaration in another window")))

(defun cb-sql-find-attrs-with-type--insert-search-result-button (result)
  (let-alist result

    (insert-text-button
     (format "%s:" .line)
     'action 'cb-sql-find-attrs-with-type--select-button
     'face (list :foreground "lightgray")
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to attribute in another window")

    (insert-text-button
     (concat " " .text)
     'action 'cb-sql-find-attrs-with-type--select-button
     'face nil
     'buffer .buffer
     'position .pos
     'mouse-face 'highlight
     'help-echo
     "mouse-2: go to attribute in another window")

    (newline)))

(defun cb-sql-find-attrs-with-type--select-button (button)
  (interactive (list (text-properties-at (point))))
  (let ((pt (button-get button 'position))
        (buf (button-get button 'buffer)))
    (with-current-buffer (pop-to-buffer buf)
      (goto-char pt))))

;;;###autoload
(defun cb-sql-find-attrs-with-type (type-names)
  (interactive (list (let* ((prompt (if sql/last-type-search-query
                                        (format "Search by type name (default: %s): "
                                                (s-join "|" sql/last-type-search-query))
                                      "Search by type name: "))
                            (input (s-split (rx (or space "|"))
                                            (read-string prompt nil nil sql/last-type-search-query))))
                       (setq sql/last-type-search-query input)
                       input)))

  (let ((matches (cb-sql-find-attrs-with-type--search-in-buffer type-names (current-buffer))))
    (cb-sql-find-attrs-with-type--display-results (s-join "|" type-names) matches)))

(provide 'cb-sql-find-attrs-with-type)

;;; cb-sql-find-attrs-with-type.el ends here
