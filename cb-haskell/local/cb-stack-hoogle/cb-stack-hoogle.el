;;; cb-stack-hoogle.el --- Adapt haskell-hoogle to use stack.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0") (dash "2.12.1") (haskell-mode "20160804.216"))

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
(require 'haskell-hoogle)

(defun cb-stack-hoogle--read-query ()
  (let ((def (haskell-ident-at-point)))
    (read-string (if def (format "Hoogle query (default %s): " def) "Hoogle query: ")
                 nil nil def)))

(defun cb-stack-hoogle--string-is-haskell-source? (s)
  (or (s-contains? "::" s)
      (s-matches? (rx bol (or "class" "module" "data") eow) s)))

(defun cb-stack-hoogle--indent-section (start end)
  (indent-region start end 2)
  (goto-char end)
  (fill-region start end))

(defun cb-stack-hoogle--insert-header (s)
  (newline)
  (insert (propertize s 'font-lock-face 'font-lock-comment-face))
  (newline))

(defun cb-stack-hoogle--prettify-hoogle-info-results ()
  (goto-char (point-min))
  (unless (s-matches? "No results found" (buffer-substring (line-beginning-position) (line-end-position)))
    (save-excursion
      (cb-stack-hoogle--insert-header "Definition")
      (newline)
      (cb-stack-hoogle--indent-section (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))
      (newline)

      (cb-stack-hoogle--insert-header "Module")
      (forward-line)
      (cb-stack-hoogle--indent-section (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))

      (unless (s-blank? (s-trim (buffer-substring (point) (point-max))))
        (newline)
        (cb-stack-hoogle--insert-header "Description")))))

;;;###autoload
(defun cb-stack-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then hoogle is asked to show
extra info for the items matching QUERY.."
  (interactive (list (cb-stack-hoogle--read-query) current-prefix-arg))
  (let ((command (format "stack hoogle -- --colour %s %s"
                         (if info " -i " "")
                         (shell-quote-argument query))))
    (with-help-window "*stack hoogle*"
      (with-current-buffer standard-output
        (prettify-symbols-mode +1)
        (insert (shell-command-to-string command))
        (when info
          (cb-stack-hoogle--prettify-hoogle-info-results))))))

;;;###autoload
(defun cb-stack-hoogle-info-at-pt ()
  "Show info for the identifier at point using Hoogle."
  (interactive)
  (-if-let (query (haskell-ident-at-point))
      (cb-stack-hoogle query t)
    (user-error "No identifier at point")))

(provide 'cb-stack-hoogle)

;;; cb-stack-hoogle.el ends here
