;;; cb-js-smart-ops.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (dash "2.12.1") (dash-functional "20160615.1351") (smartparens "20160721.1448"))

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
(require 'dash-functional)
(require 's)
(require 'smart-ops)

(autoload 'sp-get-enclosing-sexp "smartparens")

(defun cb-js-smart-ops--inside-angles? (&rest _)
  ;; HACK: smartparens doesn't always detect angles correctly, so just
  ;; look for the start of a tag on the current line.
  (or (s-matches? (rx bol (* space) "<")
                  (buffer-substring (line-beginning-position)
                                    (line-end-position)))
      (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
        (equal op "<"))))

(defun cb-js-smart-ops--inside-squares? (&rest _)
  (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
    (equal op "[")))

(defun cb-js-smart-ops--move-between-angles-insert-slash (&rest _)
  (when (smart-ops-after-match? (rx "<" (* space) ">"))
    (search-backward ">")
    (delete-horizontal-space)
    (unless (s-matches? ":" (buffer-substring (line-beginning-position) (point)))
      (save-excursion
        (insert " /")))))

(define-smart-ops-for-mode 'cb-web-html-mode
  (smart-op "<>"
            :pad-before nil :pad-after nil
            :action #'cb-js-smart-ops--move-between-angles-insert-slash))

(define-smart-ops-for-mode 'cb-web-json-mode
  (smart-ops ":" "," :pad-before nil))

(defconst cb-js-smart-ops-js-smart-ops
  (list
   (smart-op "*" :pad-before-unless (smart-ops-after-match? (rx (or "yield" "function"))))
   (smart-op "<>"
             :pad-before nil :pad-after nil
             :action #'cb-js-smart-ops--move-between-angles-insert-slash)

   (smart-ops "<" ">" :pad-unless #'cb-js-smart-ops--inside-angles?)

   (smart-ops "=" "/" :pad-unless (-orfn #'cb-js-smart-ops--inside-squares?
                                         #'cb-js-smart-ops--inside-angles?))

   ;; KLUDGE: This is the pair when inserting <tag attr=|/>
   (smart-ops "=/>" "=/>,"
              :pad-before nil
              :pad-after nil
              :action
              (lambda (&rest _)
                (skip-chars-backward ">/ ")
                (just-one-space)
                (backward-char)))

   ;; KLUDGE: This is the pair when inserting = <|>
   (smart-ops "=<>"
              :pad-after nil
              :action
              (lambda (&rest _)
                (skip-chars-backward "<>")
                (just-one-space)
                (search-forward "<")))

   ;; KLUDGE: This is the pair when inserting e.g. foo: Array<|> =
   (smart-ops "<>="
              :pad-before nil
              :action
              (lambda (&rest _)
                (skip-chars-backward "=")
                (just-one-space)
                (search-backward ">")))

   ;; KLUDGE: Could be an arrow, but could be inserting an attribute
   ;; inside a tag.
   (smart-ops "=>"
              :pad-before-unless #'cb-js-smart-ops--inside-angles?
              :action
              (lambda (&rest _)
                (when (cb-js-smart-ops--inside-angles?)
                  (skip-chars-backward "> ")
                  (just-one-space)
                  (backward-char))))

   ;; KLUDGE: Handle type annotation insertion.
   (smart-ops ":," ":="
              :pad-before nil
              :action (lambda (&rest _)
                        (skip-chars-backward ",=")
                        (just-one-space)))
   (smart-ops ":=>"
              :pad-before nil
              :action (lambda (&rest _)
                        (skip-chars-backward "=>")
                        (just-one-space)
                        (save-excursion
                          (insert " "))))

   (smart-ops ":*=>"
              :pad-before nil
              :action (lambda (&rest _)
                        (skip-chars-backward "*=>")
                        (just-one-space)
                        (save-excursion
                          (forward-char)
                          (just-one-space))))

   (smart-ops ";" ":" "," :pad-before nil)
   (smart-ops "++" "--" "++;" "--;" :pad-before nil :pad-after nil)
   (smart-ops ">=")
   (smart-op "!" :bypass? t)
   (smart-ops-default-ops :pad-unless #'cb-js-smart-ops--inside-angles?)))

(defun cb-js-smart-ops-init ()
  (apply #'define-smart-ops-for-mode 'cb-web-js-mode cb-js-smart-ops-js-smart-ops)
  (apply #'define-smart-ops-for-mode 'nodejs-repl-mode cb-js-smart-ops-js-smart-ops)
  (add-hook 'nodejs-repl-mode-hook #'smart-ops-mode))

(provide 'cb-js-smart-ops)

;;; cb-js-smart-ops.el ends here
