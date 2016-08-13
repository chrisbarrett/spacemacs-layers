;;; cb-flow-checker.el --- A flycheck checker for Flow.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (dash "2.12.1"))

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

;; A flycheck checker that provides Elm-style friendly messages for Flow.

;;; Code:

(require 'dash)
(require 'flycheck)
(require 's)
(require 'json)

(defvar cb-flow-checker--logging-verbosity 2
  "Set to zero to disable logging.")

(defun cb-flow-checker--type-error-message (msg type-expected type-actual)
  (format "%s.

As I infer the types of values in this program, I see a conflict
between these two types.

The expected type was:

    %s

But the actual type I inferred was:

    %s" msg type-actual type-expected))

(defun cb-flow-checker--intersection-type-error-message (context type-1 type-2)
  (format "Cannot combine types `%s' and `%s'.

As I infer the types of values in this program, I see an attempt
to combine these two types that cannot be satisfied.%s"
          type-1
          type-2
          (if (s-contains? "`" context)
              "\n\nCall `toString()' explicitly when interpolating values into strings."
            "")))

(defun cb-flow-checker--ident-already-bound-error-message (name)
  (format "The identifier `%s' is already bound.

As I parse your program I find an attempt to bind a name that
already exists.

Rename the previous binding or choose a different name."
          name))

(defun cb-flow-checker--unify-implicit-undefined-error-message (type)
  (format "This function implicitly returns undefined.

As I infer the types of values in this program, I see this
function can returned undefined in some branches, but here it
returns a value of type `%s'.

To fix this function
  - check you are not missing a `return' keyword
  - ensure all branches return a value of the same type." type))

(defun cb-flow-checker--type-in-value-position-error-message (type)
  (format "Reference to type `%s' at the value level.

As I parse your program I see a reference to a type at the value
level. This is an error because the name of a type is not a
value." type))

(defun cb-flow-checker--missing-annotation-error-message ()
  "Destructuring site lacks type annotations.

As I parse your program I find a destructuring site missing type
annotations.

Add type annotations so that I can collect type information for
these bindings.")

(defun cb-flow-checker--property-on-null-value-error-message ()
  "Accessing property on value that could be null.

As a infer the types of values in this program, I see an attempt
to access a property on a value which could be null.

To prove that the value is not null

  - check it explicitly using `if', or
  - use it as the first argument to a ternary expression.")

(defun cb-flow-checker--property-on-undefined-value-error-message ()
  "Accessing property on value that could be undefined.

As a infer the types of values in this program, I see an attempt
to access a property on a value which could be undefined.

To prove that the value is not undefined

  - check it explicitly using `if', or
  - use it as the first argument to a ternary expression.")

(defun cb-flow-checker--unexpected-token-error-message (msg)
  (let ((tok (-last-item (s-split (rx space) msg))))
    (format "Unexpected token: %s

As I parse your program I encounter a token that is not valid
at that location, which prevents me from continuing.

The error is near the marked location." tok)))

(defun cb-flow-checker--unresolved-type-error-message (msg)
  (format "Unknown type `%s'.

As I parse your program I encounter a type name that I cannot
find the definition for.

Import the type if it exists or write a suitable type
definition." msg))

(defun cb-flow-checker--unresolved-identifier-error-message (msg)
  (format "Unknown identifier `%s'.

As I parse your program I encounter a identifier name that I
cannot find the definition for.

Import the identifier if it exists or write a suitable identifier
definition." msg))

(defun cb-flow-checker--property-not-found-error-message (property type)
  (format "Property `%s' not defined for type `%s'.

As a infer the types of values in this program, I see an attempt
to access a property which I cannot prove to be defined.

Since I cannot prove that values of type `%s' have this property,
I must consider this an error." property type type))

(defun cb-flow-checker--type-error (level msg checker msgs)
  (-let [[(&alist 'descr type-expected
                  'loc (&alist 'start (&alist 'line line-expected 'column col-expected)
                               'source source-expected))
          _
          (&alist 'descr type-actual
                  'loc (&alist 'start (&alist 'line line-actual 'column col-actual)
                               'source source-actual))]
         msgs]
    (if (s-contains? "too few arguments" type-expected)
        (list
         (flycheck-error-new-at line-expected col-expected level
                                (cadr (s-match (rx bos (+? nonl) "(" (group (+? nonl)) ")" eos) type-expected))
                                :checker checker
                                :filename source-expected))
      (list
       (flycheck-error-new-at line-expected col-expected level
                              (cb-flow-checker--type-error-message msg type-expected type-actual)
                              :checker checker
                              :filename source-expected)
       (flycheck-error-new-at line-actual col-actual 'info
                              "A type error I detected arose from the type constraint here."
                              :checker checker
                              :filename source-actual)))))

(defun cb-flow-checker--intersection-type-error (level checker msgs)
  (-let [[(&alist 'descr type-1
                  'context context
                  'loc (&alist 'start (&alist 'line line-1 'column col-1)
                               'source source-1))
          _
          (&alist 'descr type-2
                  'loc (&alist 'start (&alist 'line line-2 'column col-2)
                               'source source-2))]
         msgs]
    (list
     (flycheck-error-new-at line-1 col-1 level
                            (cb-flow-checker--intersection-type-error-message context type-1 type-2)
                            :checker checker
                            :filename source-1)
     (flycheck-error-new-at line-2 col-2 'info
                            "A type error I detected arose from the type constraint here."
                            :checker checker
                            :filename source-2))))

(defun cb-flow-checker--ident-already-bound-error (level checker msgs)
  (-let [[(&alist 'descr name
                  'loc (&alist 'start (&alist 'line line-1 'column col-1)
                               'source source-1))
          _
          (&alist 'loc (&alist 'start (&alist 'line line-2 'column col-2)
                               'source source-2))]
         msgs]
    (list
     (flycheck-error-new-at line-1 col-1 level
                            (cb-flow-checker--ident-already-bound-error-message name)
                            :checker checker
                            :filename source-1)
     (flycheck-error-new-at line-2 col-2 'info
                            "An identifier was already bound here."
                            :checker checker
                            :filename source-2))))

(defun cb-flow-checker--property-not-found-error (level checker msgs)
  (-let* (([(&alist 'descr prop-descr
                    'loc (&alist 'start (&alist 'line line-expected 'column col-expected)
                                 'source source))
            _
            (&alist 'descr type)]
           msgs)
          ((_ prop) (s-match (rx "property `" (group (+ (not (any "`")))))
                             prop-descr)))
    (list
     (flycheck-error-new-at line-expected col-expected level
                            (cb-flow-checker--property-not-found-error-message prop type)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--parse-unify-implicit-undefined-error (level checker msgs)
  (-let [[(&alist 'descr type
                  'loc (&alist 'start (&alist 'line line 'column col)
                               'source source))]
         msgs]
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--unify-implicit-undefined-error-message type)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--missing-annotation-error (level checker msgs)
  (-let [[(&alist 'loc
                  (&alist 'start (&alist 'line line 'column col)
                          'source source))]
         msgs]
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--missing-annotation-error-message)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--unresolved-type-error (level checker msgs)
  (-let* (([(&alist 'descr desc
                    'loc
                    (&alist 'start (&alist 'line line 'column col)
                            'source source))]
           msgs)
          ((_ type) (s-match (rx "identifier `" (group (+ (not (any "`")))))
                             desc)))
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--unresolved-type-error-message type)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--unresolved-identifier-error (level checker msgs)
  (-let* (([(&alist 'descr desc
                    'loc
                    (&alist 'start (&alist 'line line 'column col)
                            'source source))]
           msgs)
          ((_ identifier) (s-match (rx "identifier `" (group (+ (not (any "`")))))
                                   desc)))
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--unresolved-identifier-error-message identifier)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--unexpected-token-error (checker msgs)
  (-let [[(&alist 'descr descr
                  'loc (&alist 'start (&alist 'line msg-line 'column msg-col)
                               'source source))]
         msgs]

    (list
     (flycheck-error-new-at msg-line msg-col 'error
                            (cb-flow-checker--unexpected-token-error-message descr)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--type-in-value-position-error (level checker msgs)
  (-let [[(&alist 'descr type
                  'loc (&alist 'start (&alist 'line line 'column col)
                               'source source))]
         msgs]
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--type-in-value-position-error-message type)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--property-on-null-value-error (level checker msgs)
  (-let [[(&alist 'loc (&alist 'start (&alist 'line line 'column col)
                               'source source))]
         msgs]
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--property-on-null-value-error-message)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--property-on-undefined-value-error (level checker msgs)
  (-let [[(&alist 'loc (&alist 'start (&alist 'line line 'column col)
                               'source source))]
         msgs]
    (list
     (flycheck-error-new-at line col level
                            (cb-flow-checker--property-on-undefined-value-error-message)
                            :checker checker
                            :filename source))))

(defun cb-flow-checker--message-comments (msgs)
  (--map (if (listp it)
             (-let [(&alist 'descr d) it] d)
           it)
         msgs))

(defun cb-flow-checker--parse-error-entry (entry checker)
  (-let* (((&alist 'level level 'message msgs) entry)
          (level (intern level))
          (comments (cb-flow-checker--message-comments msgs)))
    (cond
     ((member "This type is incompatible with the expected return type of" comments)
      (cb-flow-checker--type-error level "Type error with expected return type" checker msgs))
     ((member "This type is incompatible with an implicitly-returned undefined." comments)
      (cb-flow-checker--parse-unify-implicit-undefined-error level checker msgs))
     ((member "This type is incompatible with" comments)
      (cb-flow-checker--type-error level "Type error in argument" checker msgs))

     ((--any? (s-starts-with-p "Unexpected token" it) comments)
      (cb-flow-checker--unexpected-token-error checker msgs))

     ((member "Property not found in" comments)
      (cb-flow-checker--property-not-found-error level checker msgs))

     ((member "Property cannot be accessed on possibly null value" comments)
      (cb-flow-checker--property-on-null-value-error level checker msgs))

     ((member "Property cannot be accessed on possibly undefined value" comments)
      (cb-flow-checker--property-on-undefined-value-error level checker msgs))

     ((member "Missing annotation" comments)
      (cb-flow-checker--missing-annotation-error level checker msgs))

     ((and (member "Could not resolve name" comments)
           (--any? (s-starts-with? "type " it) comments))
      (cb-flow-checker--unresolved-type-error level checker msgs))

     ((and (member "Could not resolve name" comments)
           (--any? (s-starts-with? "identifier " it) comments))
      (cb-flow-checker--unresolved-identifier-error level checker msgs))


     ((member "This type cannot be added to" comments)
      (cb-flow-checker--intersection-type-error level checker msgs))

     ((member "type referenced from value position" comments)
      (cb-flow-checker--type-in-value-position-error level checker msgs))

     ((member "name is already bound" comments)
      (cb-flow-checker--ident-already-bound-error level checker msgs))

     (t
      (when (<= 1 cb-flow-checker--logging-verbosity)
        ;; If this branch gets used, a new handler should be implemented.
        (display-warning "Unknown Flow error" (pp-to-string msgs)))

      nil))))

(defvar-local cb-flow-checker--prev-output nil)

(defun cb-flow-checker--display-output (json)
  (with-current-buffer (get-buffer-create "*flow output*")
    (save-excursion
      (unless (equal json cb-flow-checker--prev-output)
        (setq cb-flow-checker--prev-output json)
        (erase-buffer)
        (insert json)
        (json-pretty-print-buffer)))))

(defun cb-flow-checker--error-parser (output checker _buffer)
  (-let [(&alist 'errors errors) (json-read-from-string output)]
    (when (>= 2 cb-flow-checker--logging-verbosity)
      (cb-flow-checker--display-output output))
    (-non-nil (-uniq (-flatten (--map (cb-flow-checker--parse-error-entry it checker) errors))))))


;;; Checker definition

(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)

(flycheck-define-checker javascript-flow
  "Flycheck checker for Facebook's Flow type checker for JavaScript."
  :command ("flow" "--json")
  :error-parser cb-flow-checker--error-parser
  :modes (js-mode js2-mode cb-web-js-mode)
  :predicate (lambda ()
               (locate-dominating-file default-directory ".flowconfig")))

(add-to-list 'flycheck-checkers 'javascript-flow)

(provide 'cb-flow-checker)

;;; cb-flow-checker.el ends here
