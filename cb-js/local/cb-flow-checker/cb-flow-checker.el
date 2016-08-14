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
(require 'json)
(require 's)

(defvar cb-flow-checker--logging-verbosity 2
  "Set how much additional info gets logged after type-checking.

0 = disabled
1 = show unknown errors in warning buffer
2 = show pretty-printed JSON output.")


;;; Functions for building error message strings.

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

(defun cb-flow-checker--method-call-on-null-or-undefined-error-message (method-desc)
  (-let [(_ method) (s-match (rx "`" (group (+ (not (any "`"))))) method-desc)]
    (format "Method `%s' called with value that could be null or undefined.

As I parse your program I find an attempt to call a method with a
value that could be null or undefined. This is an error because
the method's type signature specifies that this argument cannot
be null.

To prove that the value is defined

  - check it explicitly using `if', or
  - use it as the first argument to a ternary expression."
            method)))

(defun cb-flow-checker--ident-already-bound-error-message (name)
  (format "The identifier `%s' is already bound.

As I parse your program I find an attempt to bind a name that
already exists.

Rename the previous binding or choose a different name."
          name))

(defun cb-flow-checker--type-application-args-error-message (desc)
  (format "%s

As I infer the types of values in this program, I see a type
constructor applied to the wrong number of types.

Type constructors take take type names as parameters, which are
written in angle brackets. e.g. `Array<string>'.

The special token `*' can be used for type arguments that can be
inferred."
          (car (s-split "\\." desc))))

(defun cb-flow-checker--suggested-modules (module)
  (let ((m (f-filename module)))
    (->> (projectile-current-project-files)
         (--filter (equal m (f-filename (f-no-ext it))))
         (--map (concat "./" (f-relative (f-join (projectile-project-p) (f-no-ext it))
                                         default-directory))))))

(defun cb-flow-checker--module-not-found-error-message (module)
  (format "The required module `%s' was not found.

As I parse your program I find an attempt to load a module at a
given path, but it does not exist there.%s

To fix this error
  - check that the file is named correctly
  - check that the path is correct."
          module
          (-if-let (candidates (-take 3 (cb-flow-checker--suggested-modules module)))
              (concat "\n\nDid you mean one of the following?"
                      "\n  - " (s-join "\n  - " candidates))
            "")))

(defun cb-flow-checker--unexpected-ident-error-message (_)
  "Unexpected identifier.

As I parse your program I find an identifier in an unexpected
position, which prevents me from continuing.")

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

(defun cb-flow-checker--missing-annotation-error-message (_)
  "Destructuring site lacks type annotations.

As I parse your program I find a destructuring site missing type
annotations.

Add type annotations so that I can collect type information for
these bindings.")

(defun cb-flow-checker--property-on-null-or-undefined-value-error-message (_)
  "Accessing property on value that could be null or undefined.

As I infer the types of values in this program, I see an attempt
to access a property on a value which could be null or undefined.

To prove that the value is defined

  - check it explicitly using `if', or
  - use it as the first argument to a ternary expression.")

(defun cb-flow-checker--property-or-element-on-null-or-undefined-value-error-message (_)
  "Accessing property or element on a value that could be null or undefined.

As I infer the types of values in this program, I see an attempt
to access a property or element of a value which could be null or
undefined.

To prove that the value is defined

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

(defun cb-flow-checker--unresolved-identifier-error-message (desc)
  (-let [(_ identifier) (s-match (rx "identifier `" (group (+ (not (any "`")))))
                                 desc)]
    (format "Unknown identifier `%s'.

As I parse your program I encounter an identifier that I cannot
find the definition for.

Import the identifier if it exists or write a suitable identifier
definition." identifier)))

(defun cb-flow-checker--property-not-found-error-message (property type)
  (format "Property `%s' not defined for type `%s'.

As I infer the types of values in this program, I see an attempt
to access a property which I cannot prove to be defined.

Since I cannot prove that values of type `%s' have this property,
I must consider this an error." property type type))


;;; Error parsers

(defun cb-flow-checker--type-error (level msg checker msgs)
  (-let [[(&alist 'descr type-expected
                  'loc (&alist 'start (&alist 'line line-expected 'column col-expected)
                               'source source-expected))
          _
          (&alist 'descr type-actual
                  'loc (&alist 'start (&alist 'line line-actual 'column col-actual)
                               'source source-actual))]
         msgs]
    (list
     (flycheck-error-new-at line-expected col-expected level
                            (cb-flow-checker--type-error-message msg type-expected type-actual)
                            :checker checker
                            :filename source-expected)
     (flycheck-error-new-at line-actual col-actual 'info
                            "A type error I detected arose from the type constraint here."
                            :checker checker
                            :filename source-actual))))

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

(defun cb-flow-checker--single-message-error-parser (level checker msgs msg-format-fn)
  (cb-flow-checker--indexed-error-and-message-parser level checker msgs 0 0 msg-format-fn))

(defun cb-flow-checker--indexed-error-and-message-parser (level checker msgs desc-index pos-index msg-format-fn)
  "Parse a vector of notes, where the position and description are in separate notes."
  (-let (((&alist 'loc (&alist 'start (&alist 'line line 'column col)
                               'source source))
          (elt msgs pos-index))

         ((&alist 'descr descr) (elt msgs desc-index)))
    (list
     (flycheck-error-new-at line col level
                            (funcall msg-format-fn descr)
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

     ;; Parse errors

     ((--any? (s-starts-with-p "Unexpected token" it) comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--unexpected-token-error-message))

     ((member "Unexpected identifier" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--unexpected-ident-error-message))

     ;; Type errors

     ((member "This type is incompatible with the expected return type of" comments)
      (cb-flow-checker--type-error level "Type error with expected return type" checker msgs))

     ((member "This type is incompatible with an implicitly-returned undefined." comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--unify-implicit-undefined-error-message))

     ((member "This type is incompatible with" comments)
      (cb-flow-checker--type-error level "Type error in argument" checker msgs))

     ((member "Method cannot be called on possibly null value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--method-call-on-null-or-undefined-error-message))

     ((member "Method cannot be called on possibly undefined value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--method-call-on-null-or-undefined-error-message))

     ((member "Property not found in" comments)
      (cb-flow-checker--property-not-found-error level checker msgs))

     ((member "Property cannot be accessed on possibly null value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--property-on-null-or-undefined-value-error-message))

     ((member "Property cannot be accessed on possibly undefined value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--property-on-null-or-undefined-value-error-message))

     ((member "Computed property/element cannot be accessed on possibly undefined value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--property-or-element-on-null-or-undefined-value-error-message))

     ((member "Computed property/element cannot be accessed on possibly null value" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--property-or-element-on-null-or-undefined-value-error-message))

     ((member "Missing annotation" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--missing-annotation-error-message))

     ((member "Required module not found" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--module-not-found-error-message))

     ((and (member "Could not resolve name" comments)
           (--any? (s-starts-with? "type " it) comments))
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--unresolved-type-error-message))

     ((and (member "Could not resolve name" comments)
           (--any? (s-starts-with? "identifier " it) comments))
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--unresolved-identifier-error-message))

     ((member "This type cannot be added to" comments)
      (cb-flow-checker--intersection-type-error level checker msgs))

     ((member "type referenced from value position" comments)
      (cb-flow-checker--single-message-error-parser level checker msgs
                                     #'cb-flow-checker--type-in-value-position-error-message))

     ((member "name is already bound" comments)
      (cb-flow-checker--ident-already-bound-error level checker msgs))

     ((--any? (s-starts-with? "Application of polymorphic type needs" it) comments)
      (cb-flow-checker--indexed-error-and-message-parser level checker msgs 1 0
                                          #'cb-flow-checker--type-application-args-error-message))

     (t
      (when (<= 1 cb-flow-checker--logging-verbosity)
        ;; If this branch gets used, a new handler should be implemented.
        (display-warning "Unknown Flow error" (pp-to-string msgs)))

      nil))))


;;; Logger utilities

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
