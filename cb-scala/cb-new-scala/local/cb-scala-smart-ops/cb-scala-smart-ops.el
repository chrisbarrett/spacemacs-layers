;;; cb-scala-smart-ops.el --- <enter description here>  -*- lexical-binding: t; -*-

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
(require 'smart-ops)

(defun cb-scala-smart-ops--open-line-below-current-indentation ()
  "Open a new line below at the current indent level."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (indent-to col)))

(defun cb-scala-smart-ops--replace-slashes-with-doc ()
  (atomic-change-group
    (delete-region (line-beginning-position) (line-end-position))
    (cb-scala-smart-ops--insert-scaladoc)))

(defun cb-scala-smart-ops--insert-scaladoc ()
  "Insert the skeleton of a ScalaDoc at point."
  (interactive "*")
  (indent-for-tab-command) (insert "/**")
  (cb-scala-smart-ops--open-line-below-current-indentation) (insert " * ")
  (save-excursion
    (cb-scala-smart-ops--open-line-below-current-indentation) (insert "*/")))

(defun cb-scala-smart-ops--at-repl-prompt? ()
  (s-matches? (rx bol (* space) "scala>" (* space))
              (buffer-substring (line-beginning-position) (point))))

(defun cb-scala-smart-ops-init ()
  (let ((ops (-flatten-n 1
                         (list
                          (smart-ops "???" "?" "=" "==" "+" "-" "*" "/" "<" ">" "|" "$" "&" "%" "!" "~")
                          (smart-ops ":" "," :pad-before nil)
                          (smart-ops "@" :pad-after nil)
                          (smart-ops ":=")

                          ;; Inserting this op means you're probably editing a
                          ;; function return type. Pad internally and move point
                          ;; inside.
                          (let ((inserting-type? (smart-ops-before-match? (rx bos (* space) "="))))
                            (smart-ops ":="
                                       :action
                                       (lambda (&rest _)
                                         (when (funcall inserting-type? (point))
                                           (just-one-space)
                                           (save-excursion
                                             (insert " ")
                                             (search-backward ":")
                                             (delete-horizontal-space))))))


                          ;; Reformat ':_*' as ': _*'
                          (smart-ops ":_*"
                                     :pad-before nil
                                     :pad-after nil
                                     :action
                                     (lambda (&rest _)
                                       (save-excursion
                                         (search-backward "_")
                                         (just-one-space))))
                          (smart-ops ":=>"
                                     :pad-before nil
                                     :action
                                     (lambda (&rest _)
                                       (save-excursion
                                         (search-backward "=")
                                         (just-one-space))))
                          (smart-ops "_=>"
                                     :action
                                     (lambda (&rest _)
                                       (save-excursion
                                         (search-backward "=")
                                         (just-one-space))))


                          ;; Prevent above smart ops from breaking underscores in
                          ;; symbols.
                          (smart-ops "_" :bypass? t)
                          (smart-ops "__" :bypass? t)
                          (smart-ops "___" :bypass? t)

                          ;; Reformat '=???' as '= ???'
                          (smart-ops "=???" "=???,"
                                     :action
                                     (lambda (&rest _)
                                       (save-excursion
                                         (skip-chars-backward ",? ")
                                         (just-one-space))))))))

    (define-smart-ops-for-mode 'scala-mode
      (smart-op "///" :action 'cb-scala-smart-ops--replace-slashes-with-doc)
      ops)

    (define-smart-ops-for-mode 'sbt-file-mode
      (smart-op "///" :action 'cb-scala-smart-ops--replace-slashes-with-doc)
      ops)

    (define-smart-ops-for-mode 'ensime-inf-mode
      (smart-op ":"
                :pad-before nil
                :pad-after-unless
                (lambda (_)
                  (forward-char -1)
                  (cb-scala-smart-ops--at-repl-prompt?)))
      ops)))

(provide 'cb-scala-smart-ops)

;;; cb-scala-smart-ops.el ends here
