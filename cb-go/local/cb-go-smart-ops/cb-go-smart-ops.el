;;; cb-go-smart-ops.el --- Smart operator defs for Golang.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <admiral@walrus.cool>

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

;; Aped from my smart ops for Rust.

;;; Code:

(require 'smart-ops)

(defconst cb-go-smart-ops
  (list
   (smart-ops ";" "~" ":" "," :pad-before nil)
   (smart-ops "&" "!" :bypass? t)
   (smart-ops ";" "~" ":" "," :pad-before nil)

   ;; References

   (smart-ops ":&" ",&"
              :pad-before nil
              :pad-after nil
              :action
              (lambda (&rest _)
                (save-excursion
                  (skip-chars-backward "&")
                  (just-one-space))))

   ;; Dereferencing

   (smart-ops "*"
              :pad-after-unless
              (lambda (end)
                (save-excursion
                  (goto-char (1- end))
                  (smart-ops--line-empty-up-to-point?))))

   (smart-ops ":=*" "=*" "|*" :pad-after nil
              :action
              (lambda (&rest _)
                (save-excursion
                  (skip-chars-backward "*")
                  (just-one-space))))

   (smart-ops ",*" ",&" :pad-before nil :pad-after nil
              :action
              (lambda (&rest _)
                (save-excursion
                  (skip-chars-backward "* ")
                  (just-one-space))))

   ;; Assignments

   (smart-ops "=&" "=*" ":=*" ":=&" ":=<-" "=<-"
              :pad-after nil
              :action
              (lambda (&rest _)
                (save-excursion
                  (search-backward "=")
                  (forward-char 1)
                  (just-one-space))))

   (smart-ops-default-ops)))

;;;###autoload
(defun cb-go-smart-ops-init ()
  (apply #'define-smart-ops-for-mode 'go-mode cb-go-smart-ops))

(provide 'cb-go-smart-ops)

;;; cb-go-smart-ops.el ends here
