;;; cb-haskell-smart-ops.el --- Smart operators for Haskell.  -*- lexical-binding: t; -*-

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
(require 'smart-ops nil t)

(autoload 'haskell-indentation-indent-line "haskell-indentation")
(autoload 'shm-current-node "shm-ast")
(autoload 'sp-get-enclosing-sexp "smartparens")

(defun cb-haskell-smart-ops--reformat-comment-at-point ()
  (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
    (when (and (equal op "{")
               (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                           (buffer-substring beg end)))
      (goto-char beg)
      (delete-region beg end)
      (insert "{- ")
      (save-excursion (insert " -}")))))

(defun cb-haskell-smart-ops--at-pragma? (&rest _)
  (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
    (and (equal op "{")
         (s-matches? (rx bos "{" (* (any "-" space "#")) "}" eos)
                     (buffer-substring beg end)))))

(defun cb-haskell-smart-ops--reformat-pragma-at-point ()
  (-when-let ((&plist :beg beg :end end) (sp-get-enclosing-sexp))
    (goto-char beg)
    (delete-region beg end)
    (insert "{-# ")
    (save-excursion (insert " #-}"))))

(defun cb-haskell-smart-ops--indent-if-in-exports ()
  (when (ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
    (haskell-indentation-indent-line)))

(defun cb-haskell-smart-ops--ops ()
  (-flatten-n 1
              (list
               (smart-ops "." :bypass? t)
               (smart-ops "->" "=>")
               (smart-op "::!"
                         :pad-before t
                         :pad-after nil
                         :action
                         (lambda ()
                           (save-excursion
                             (skip-chars-backward "!")
                             (just-one-space))))

               (smart-ops "$" "=" "~" "^" ":" "?")
               (smart-ops "^." ".~" "^~" "%~" :pad-before t :pad-after t)
               (smart-op ";"
                         :pad-before nil :pad-after t)
               (smart-ops ","
                          :pad-before nil :pad-after t
                          :action
                          #'cb-haskell-smart-ops--indent-if-in-exports)
               (smart-op "-"
                         :action #'cb-haskell-smart-ops--reformat-comment-at-point)
               (smart-op "#"
                         :pad-before-unless #'cb-haskell-smart-ops--at-pragma?
                         :pad-after-unless #'cb-haskell-smart-ops--at-pragma?
                         :action #'cb-haskell-smart-ops--reformat-pragma-at-point)
               (smart-ops-default-ops))))

(defun cb-haskell-smart-ops-init ()
  (with-eval-after-load 'smart-ops
    (define-smart-ops-for-mode 'haskell-mode
      (cb-haskell-smart-ops--ops))))

(provide 'cb-haskell-smart-ops)

;;; cb-haskell-smart-ops.el ends here
