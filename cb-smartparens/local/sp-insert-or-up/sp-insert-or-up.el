;;; sp-insert-or-up.el --- Move up and reformat parens when closing. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((smartparens "20160324.1541") (bind-key "1.0"))

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

;; Move up and reformat parens when closing.

;;; Code:

(autoload 'sp-up-sexp "smartparens")
(autoload 'bind-key "bind-key")

(defconst sp-insert-or-up-keys '(")" "]" "}"))

;;;###autoload
(defun sp-insert-or-up (delim &optional arg)
  "Insert a delimiter DELIM if inside a string, else move up.
Prefix ARG is passed to `sp-up-sexp'."
  (interactive "sDelimiter:\nP")
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?
           (insert delim))
          ((and (boundp 'smartparens-mode) smartparens-mode)
           (sp-up-sexp arg 'interactive))
          (t
           (insert delim)))))

(defun sp-insert-or-up--mk-insert-or-up-for-key (key)
  (lambda (&optional arg)
    "Insert a closing pair or navigate up."
    (interactive "P")
    (with-demoted-errors
        (sp-insert-or-up key arg))))

(defun sp-insert-or-up--set-keys ()
  (dolist (key sp-insert-or-up-keys)
    (dolist (map (list smartparens-mode-map smartparens-strict-mode-map))
      (bind-key (kbd key) (sp-insert-or-up--mk-insert-or-up-for-key key) map))))

;;;###autoload
(defun sp-insert-or-up-init ()
  (add-hook 'smartparens-mode-hook #'sp-insert-or-up--set-keys)
  (add-hook 'smartparens-strict-mode-hook #'sp-insert-or-up--set-keys))

(provide 'sp-insert-or-up)

;;; sp-insert-or-up.el ends here
