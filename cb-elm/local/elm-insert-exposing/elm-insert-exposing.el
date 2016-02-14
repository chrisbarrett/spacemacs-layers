;;; elm-insert-exposing.el --- Insert "exposing" after an insert. -*- lexical-binding: t; -*-

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

(require 'elm-mode)
(require 's)

(autoload 'evil-insert-state "evil-states")

(defun elm-insert-exposing--at-import-line? ()
  (s-matches? "^import " (buffer-substring (line-beginning-position) (point))))

(defun elm-insert-exposing--at-import-line-with-exposing? ()
  (when (elm-insert-exposing--at-import-line?)
    (s-matches? "exposing" (buffer-substring (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun elm-insert-exposing ()
  "Insert an exposing directive to an import.

If the import already has an exposing directive, insert a new entry.

If not at an import, run `elm-repl-push-decl'."
  (interactive)
  (cond
   ;; Update an existing exposing list.
   ((elm-insert-exposing--at-import-line-with-exposing?)
    (goto-char (line-end-position))
    (search-backward ")")
    (unless (thing-at-point-looking-at (rx "(" (* space)))
      (delete-horizontal-space)
      (insert ", "))
    (evil-insert-state))

   ;; Add a new exposing list.
   ((elm-insert-exposing--at-import-line?)
    (goto-char (line-end-position))
    (just-one-space)
    (insert "exposing ()")
    (forward-char -1)
    (evil-insert-state))

   (t
    (elm-repl-push-decl))))

(defun elm-insert-exposing-init ()
  (define-key elm-mode-map (kbd "C-c C-e") #'elm-insert-exposing))

(provide 'elm-insert-exposing)

;;; elm-insert-exposing.el ends here
