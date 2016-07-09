;;; sp-generic-prog.el --- Generic smartparens commands for prog mode.  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 's)
(require 'smartparens)

(autoload 'smart-ops-backspace "smart-ops")
(autoload 'evil-define-key "evil-core")

(defun sp-generic-prog--in-string-or-comment? ()
  "Non-nil if point is at a string or comment."
  (or (nth 8 (syntax-ppss))
      (-intersection (list font-lock-comment-face font-lock-doc-face font-lock-string-face)
                     (face-at-point nil t))))

(defun sp-generic-prog--current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun sp-inside-curly-braces? (&optional same-line? sexp)
  (sp-inside-sexp? "{" same-line? sexp))

(defun sp-inside-square-braces? (&optional same-line? sexp)
  (sp-inside-sexp? "[" same-line? sexp))

(defun sp-inside-sexp? (expected-op &optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op actual-op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal expected-op actual-op)
      (if same-line?
          (= (line-number-at-pos beg) (line-number-at-pos end))
        t))))

(defun sp-inside-curly-braces-no-content? (&optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal op "{")
      (and (s-blank? (buffer-substring (1+ beg) (1- end)))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp-inside-square-braces-no-content? (&optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal op "[")
      (and (s-blank? (buffer-substring (1+ beg) (1- end)))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp-inside-empty-angle-braces-no-content? ()
  (and (equal (char-before) ?<)
       (equal (char-after) ?>)))

(defun sp-inside-empty-angle-braces-empty-content? ()
  (let ((before?
         (save-excursion
           (skip-chars-backward " \t")
           (equal (char-before) ?<)))
        (after?
         (save-excursion
           (skip-chars-forward " \t")
           (equal (char-after) ?>))))
    (and before? after?)))

(defun sp-inside-curly-braces-blank-content? (&optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal op "{")
      (and (s-blank? (s-trim (buffer-substring (1+ beg) (1- end))))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp-inside-square-braces-blank-content? (&optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal op "[")
      (and (s-blank? (s-trim (buffer-substring (1+ beg) (1- end))))
           (if same-line?
               (= (line-number-at-pos beg) (line-number-at-pos end))
             t)))))

(defun sp-inside-curly-braces-with-content? (&optional same-line? sexp)
  (and (sp-inside-curly-braces? same-line?) (not (sp-inside-curly-braces-no-content? same-line? sexp))))

(defun sp-beg (&optional sexp)
  (plist-get (or sexp (sp-get-enclosing-sexp )) :beg))

(defun sp-just-after-open-op? (op)
  (s-matches? (rx-to-string `(and ,op (* space) eos)) (buffer-substring (line-beginning-position) (point))))

(defun sp-split-braced-expression-over-new-lines (&optional statement-delimiter-rx sexp)
  "Split the braced expression on the current line over several lines.

Optionally split the internal lines according to the given regexp
STATEMENT-DELIMITER-RX.

SEXP is the output from `sp-get-enclosing-sexp'."
  (-let [(&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (newline-and-indent)
      (let ((beg (sp-beg)))
        (while (and (search-forward-regexp statement-delimiter-rx nil t)
                    (-when-let (beg2 (sp-beg))
                      (<= beg beg2)))
          (when (equal beg (sp-beg))
            (unless (sp-generic-prog--in-string-or-comment?)
              (insert "\n")
              (indent-according-to-mode))))))

    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (sp-just-after-open-op? op)
      (forward-line)
      (back-to-indentation))))

;;;###autoload
(defun sp-generic-prog-backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (-if-let (sexp (ignore-errors (sp-get-enclosing-sexp)))
      (cond
       ((sp-inside-empty-angle-braces-no-content?)
        (delete-char 1)
        (delete-char -1))
       ((sp-inside-empty-angle-braces-empty-content?)
        (delete-horizontal-space))

       ((or (sp-inside-curly-braces-no-content? nil sexp)
            (sp-inside-square-braces-no-content? nil sexp))
        (call-interactively 'sp-backward-delete-char))

       ((or (sp-inside-curly-braces-blank-content? t sexp)
            (sp-inside-square-braces-blank-content? t sexp))
        (delete-horizontal-space))

       ((or (sp-inside-curly-braces-blank-content? nil sexp)
            (sp-inside-square-braces-blank-content? nil sexp))
        (just-one-space -1)
        (save-excursion
          (insert " ")))

       (t
        (smart-ops-backspace)))
    (smart-ops-backspace)))

;;;###autoload
(defun sp-generic-prog-space ()
  "Insert a space, performing extra padding inside braced expressions."
  (interactive)
  (-if-let (sexp (ignore-errors (sp-get-enclosing-sexp)))
      (cond
       ((or (sp-inside-curly-braces-no-content? nil sexp)
            (sp-inside-square-braces-no-content? nil sexp))
        (delete-horizontal-space)
        (insert " ")
        (save-excursion (insert " ")))
       (t
        (insert " ")))
    (insert " ")))

;;;###autoload
(defun sp-generic-prog-ret (&optional arg)
  "Insert a newline with context-sensitive formatting.

With prefix arg ARG, just insert a newline and indent."
  (interactive "P")
  (cond
   (arg
    (newline-and-indent))
   ((sp-generic-prog--in-string-or-comment?)
    (comment-indent-new-line)
    (just-one-space))

   ((-when-let (sexp (sp-get-enclosing-sexp))
      (or (sp-inside-curly-braces? t sexp)
          (sp-inside-square-braces? t sexp)))
    (sp-split-braced-expression-over-new-lines (rx (or ";" ","))))

   (t
    (call-interactively 'comment-indent-new-line))))

;;;###autoload
(defun sp-generic-prog-kill ()
  "Kill lines, cleaning up extra spaces afterwards."
  (interactive)
  (cond
   ((s-blank? (s-trim (sp-generic-prog--current-line)))
    (kill-whole-line))
   (t
    (call-interactively 'sp-kill-sexp)

    ;; Delete extra spaces backwards.
    (when (s-matches? (rx (not space))
                      (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space t))

    ;; Join lines cleanly.
    (when (s-blank? (s-trim (sp-generic-prog--current-line)))
      (let ((pt (point)))
        (join-line)
        (goto-char (1+ pt)))))))

;;;###autoload
(defun sp-generic-prog-init ()
  (define-key prog-mode-map (kbd "<backspace>") #'sp-generic-prog-backspace)
  (define-key prog-mode-map (kbd "DEL") #'sp-generic-prog-backspace)
  (define-key prog-mode-map (kbd "SPC") #'sp-generic-prog-space)
  (define-key prog-mode-map (kbd "RET") #'sp-generic-prog-ret)

  (with-eval-after-load 'smartparens
    (define-key (with-no-warnings smartparens-mode-map) (kbd "C-k") #'sp-generic-prog-kill))

  (with-eval-after-load 'evil
    (evil-define-key 'insert prog-mode-map
      (kbd "<backspace>") #'sp-generic-prog-backspace
      (kbd "DEL") #'sp-generic-prog-backspace
      (kbd "SPC") #'sp-generic-prog-space
      (kbd "RET") #'sp-generic-prog-ret)
    (evil-define-key 'normal prog-mode-map
      (kbd "<backspace>") nil
      (kbd "DEL") nil
      (kbd "SPC") nil
      (kbd "RET") nil)))

(provide 'sp-generic-prog)

;;; sp-generic-prog.el ends here
