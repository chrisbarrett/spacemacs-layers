;;; cb-use-package-extensions.el --- Extra keywords for use-package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((use-package "20160706.1520") (dash "20160619.611"))

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

;; Adds extra keywords to `use-package'.
;;
;; 1. `:evil-bind'
;;
;;    `:evil-bind' provides a mechanism for setting up evil keybindings, similar
;;    to the `:bind' keyword.
;;
;;    The value for `:evil-bind' is a list, which should start with the keyword
;;    `:state' followed by a symbol representing the evil state for the
;;    subsequent bindings.
;;
;;      (use-package foo
;;        :evil-bind (:state normal
;;                    ("a" . foo)
;;                    ("b" . bar)))
;;
;;    Bindings are specified as `(KEY-SEQUENCE . FUNCTION)' pairs, and can be
;;    interleaved with further `:state' declarations to declare bindings for
;;    different states:
;;
;;      (use-package foo
;;        :evil-bind (:state normal
;;                    ("a" . foo)
;;                    :state insert
;;                    ("b" . bar)))
;;
;;    Bindings specified in this way are global evil keybindings, defined using
;;    `evil-global-set-key'.
;;
;;    It is possible to declare bindings for certain maps, by using the `:map'
;;    keyword followed by the name of a keymap. Subsequent keybinding pairs are
;;    defined only when that keymap is active, using `evil-define-key'.
;;
;;    The example below demonstrates setting a global binding in normal state,
;;    as well as a mode-specific binding in insert state:
;;
;;      (use-package foo
;;        :evil-bind (:state normal
;;                    ("a" . foo)
;;                    :state insert
;;                    :map foo-mode-map
;;                    ("b" . bar)))
;;
;; 2. `:leader-bind'
;;
;;    `:leader-bind' provides a way to declare Spacemacs leader key bindings. It
;;    takes a list of conses, similar to `:evil-bind'.
;;
;;      (use-package foo
;;        :leader-bind (("a" . foo)
;;                      ("b" . bar)))
;;
;;    By default, bindings are set for all modes. The `:mode' keyword and a
;;    major-mode name can be inserted into the list to set those bindings for the
;;    major mode prefix instead.
;;
;;      (use-package foo
;;        :leader-bind (("a" . foo)
;;                      :mode foo-mode
;;                      ("b" . bar)))

;;; Code:

(require 'use-package)
(require 'dash)

(defun use-package-normalize/:evil-bind (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label args)
      (use-package-normalize-pairs name label args nil t nil))))

(defun use-package-handler/:evil-bind
    (name _kw args rest state)
  (let ((commands (-keep #'cdr-safe (-filter #'consp args))))
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))

     `((ignore
        (with-eval-after-load 'evil
          ,@(->>
             ;; Traverse `args' pairwise to process keyword-value pairs.
             (-zip-pair args (-snoc (cdr args) nil))
             (--keep
              (-let (((&plist :evil-state s :map keymap) state)
                     ((fst . snd) (when (listp it) it)))
                (cond
                 ;; Assume the current item is a (:KEYWORD . VALUE) pair.  Update the state based on that value.

                 ((equal :map fst)
                  (setq state (plist-put state :map snd))
                  nil)

                 ((equal :state fst)
                  (setq state (plist-put state :evil-state snd))
                  nil)

                 ;; Assume the current item is a (VALUE@i . VALUE@i+1) pair.
                 ;; Ignore the second element and just process the first.

                 ((and (consp fst) keymap)
                  (unless s
                    (use-package-error (format "No evil state declared before: %s" fst)))
                  (-let [(k . fn) fst]
                    `(evil-define-key ',s
                       ,keymap
                       ,(if (stringp k) `(kbd ,k) k)
                       #',fn)))

                 ((consp fst)
                  (unless s
                    (use-package-error (format "No evil state declared before: %s" fst)))
                  (-let [(k . fn) fst]
                    `(evil-global-set-key ',s
                                          ,(if (stringp k) `(kbd ,k) k)
                                          #',fn)))

                 ;; A symbol is presumed to be the value of a previous kvp.

                 ((symbolp fst)
                  nil)

                 ;; Anything else is invalid.

                 (t
                  (use-package-error (format "Expected :map, :state or (KEY . FN), but got: %s"
                                             fst)))))))))))))

(defun use-package-normalize/:leader-bind (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label args)
      (use-package-normalize-pairs name label args nil t nil))))

(defun use-package-handler/:leader-bind
    (name _kw args rest state)
  (let ((commands (-keep #'cdr-safe (-filter #'consp args))))
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))

     `((ignore
        ,@(->>
           ;; Traverse `args' pairwise to process keyword-value pairs.
           (-zip-pair args (-snoc (cdr args) nil))
           (--keep
            (-let (((&plist :mode mode) state)
                   ((fst . snd) (when (listp it) it)))
              (cond
               ;; Assume the current item is a (:KEYWORD . VALUE) pair.  Update the state based on that value.
               ((equal :mode fst)
                (setq state (plist-put state :mode snd))
                nil)

               ;; Assume the current item is a (VALUE@i . VALUE@i+1) pair.
               ;; Ignore the second element and just process the first.

               ((and mode (consp fst))
                (-let [(k . fn) fst]
                  `(spacemacs/set-leader-keys-for-major-mode ',mode ,k #',fn)))

               ((consp fst)
                (-let [(k . fn) fst]
                  `(spacemacs/set-leader-keys ,k #',fn)))

               ;; A symbol is presumed to be the value of a previous kvp.

               ((symbolp fst)
                nil)

               ;; Anything else is invalid.

               (t
                (use-package-error (format "Expected :mode or (KEY . FN), but got: %s" fst))))))))))))

(add-to-list 'use-package-keywords :leader-bind t)
(add-to-list 'use-package-keywords :evil-bind t)

(provide 'cb-use-package-extensions)

;;; cb-use-package-extensions.el ends here

