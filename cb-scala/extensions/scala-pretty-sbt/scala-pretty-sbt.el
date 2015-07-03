;;; scala-pretty-sbt.el --- Pretty sbt running in comint.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Prettify SBT when running in comint.

;;; Code:

(require 'sbt-mode)

(defgroup scala-pretty-sbt nil
  "Improved font-locking for SBT buffers."
  :group 'languages
  :prefix "scala-pretty-sbt-")



(defface scala-pretty-sbt-pending-face
  '((t (:foreground "#6c71c4")))
  'scala-pretty-sbt)

(defface scala-pretty-sbt-error-face
  '((t (:foreground "#cb4b16")))
  'scala-pretty-sbt)

(defface scala-pretty-sbt-warning-face
  '((t (:foreground "#b58900")))
  'scala-pretty-sbt)

(defface scala-pretty-sbt-success-face
  '((t (:foreground "springgreen3")))
  'scala-pretty-sbt)

(defface scala-pretty-sbt-column-indicator-face
  '((t (:foreground "#b58900")))
  'scala-pretty-sbt)



(defvar scala-pretty-sbt--prompt-regexp
  (rx bol
      (group (? space)
             (or
              ;; SBT
              (and (+ (any alpha "-")) ">" (+ space))
              ;; Activator
              (and "[" (+ (any alpha "-")) "] $" (+ space))))
      (* space)))

(defun scala-pretty-sbt--apply-sbt-font-locking ()
  "Apply pretty SBT font locking to the current buffer."
  (font-lock-add-keywords
   nil
   `((,(rx bol (* space) (group "[info]" (* nonl)))
      1 'font-lock-comment-face t)

     (,scala-pretty-sbt--prompt-regexp
      1 'font-lock-type-face t)

     (,(rx bol (* space) (group "[info] "))
      (0 '(face nil invisible t)))

     ;; Pending tests
     (,(rx bol (* space) "[info]" (+ space) (group (+ nonl)) (group "(pending)") eol)
      (1 'scala-pretty-sbt-pending-face t)
      (2 '(face scala-pretty-sbt-pending-face italic t) t))

     ;; Failing tests
     (,(rx bol (* space) "[info]" (+ space) (group (+ nonl)) (group "*** FAILED ***"))
      (1 'scala-pretty-sbt-error-face t)
      (2 '(face scala-pretty-sbt-error-face italic t) t))

     ;; Ignored tests
     (,(rx bol (* space) "[info]" (+ space) (group (+ nonl)) (group "!!! IGNORED !!!") (* space) eol)
      (1 'scala-pretty-sbt-warning-face t)
      (2 '(face scala-pretty-sbt-warning-face italic t) t))

     ;; Stacktraces
     (,(rx bol (* space) (? "[error]" (* space)) (group "at " (+ nonl)))
      1 'font-lock-comment-face t)

     ;; Syntax Errors
     (,(rx bol (* space) "[error]" (* space) (group "^") (* space) eol)
      (1 '(face scala-pretty-sbt-column-indicator-face display "▲")))

     ;; Exceptions
     (,(rx bol (* space) (group (*? (any "." alpha)) ".") (group (+ alpha) "Exception") ":"
           (group (* nonl)))
      (1 '(face scala-pretty-sbt-error-face display "⚫ "))
      (2 'font-lock-type-face t)
      (3 'font-lock-string-face t))


     ;; Re-compose notification levels
     (,(rx bol (* space) (group (or "[ERROR]" "[error]")))
      (1 '(face scala-pretty-sbt-error-face display "⚫")))
     (,(rx bol (* space) (group (or "[WARN]" "[warn]")))
      (1 '(face scala-pretty-sbt-warning-face display "⚫")))
     (,(rx bol (* space) (group (or "[SUCCESS]" "[success]")))
      (1 '(face scala-pretty-sbt-success-face display "⚫")))

     ;; Akka logging

     (,(rx bol (* space) "[ERROR]" (+ space)
           (group (and "[" (+ (not (any "]"))) "]" (+ space))) ; timestamp
           (group (+ (and "[" (+ (not (any "]"))) "]" (+ space)))) ; remaining fields
           (group (* nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t)
      (3 'scala-pretty-sbt-error-face t))

     (,(rx bol (* space) "[WARN]" (+ space)
           (group (and "[" (+ (not (any "]"))) "]" (+ space))) ; timestamp
           (group (+ (and "[" (+ (not (any "]"))) "]" (+ space)))) ; remaining fields
           (group (* nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t)
      (3 'scala-pretty-sbt-warning-face t))

     (,(rx (* space) (or "[INFO]" "[DEBUG]") (+ space) (group (* nonl)))
      (0 'font-lock-comment-face t))

     (,(rx bol (* space) (group (+ (any digit ":" "."))) (group (+ nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t))

     (,(rx bol (* space)
           (? (+ (any digit ":" ".")) (+ space))
           "[" (* nonl) " INFO " (* nonl) "\n")
      (0 '(face nil invisible t)))

     ;; Download status
     (,(rx (* space) "[INFO]" (+ space) (group "[SUCCESSFUL ]"))
      (1 '(face scala-pretty-sbt-success-face display "✔")))
     (,(rx (* space) "[INFO]" (+ space) (group "downloading"))
      (1 '(face nil display "⬇")))
     )))

(defun sbt-prettify-buffer ()
  "Apply special SBT font locking to the current buffer."
  (interactive)
  (scala-pretty-sbt--apply-sbt-font-locking)
  (font-lock-fontify-buffer))

(add-hook 'sbt-mode-hook 'scala-pretty-sbt--apply-sbt-font-locking)

(provide 'scala-pretty-sbt)

;;; scala-pretty-sbt.el ends here
