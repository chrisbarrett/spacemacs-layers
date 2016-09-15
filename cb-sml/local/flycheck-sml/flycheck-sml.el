;;; flycheck-sml.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (flycheck "0.24.0))

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

(require 'flycheck)
(require 'sml-mode)
(require 's)
(require 'dash)

(flycheck-def-option-var flycheck-sml-nj-executable "sml" sml-nj
  "Command to use to run SML/NJ.")

(flycheck-def-option-var flycheck-sml-nj-args nil sml-nj
  "Run options for SML/NJ.")

(defun flycheck-sml--run ()
  (let* ((file (when (and sml-config-file (file-exists-p sml-config-file)) sml-config-file))
         (buf (apply 'make-comint (format " flycheck-sml[%s]" default-directory)
                     flycheck-sml-nj-executable
                     file
                     flycheck-sml-nj-args)))
    (with-current-buffer buf
      (inferior-sml-mode))
    buf))

(defun flycheck-sml--extract-err (str)
  (-when-let ((_input file line col msg)
              (s-match (flycheck-rx-to-string
                        '(and
                          line-start (file-name) ":" line "."  column (? "-" (+ digit) "." (+ digit))
                          (one-or-more space) (message (* anything))))
                       str))
    (list :file file
          :line (string-to-number line)
          :col (string-to-number col)
          :level (if (s-matches? "^Warning:" msg) 'warning 'error)
          :message (s-chop-prefixes '("Warning: " "Error: ") msg))))

(defun flycheck-sml--grouped-into-errs (lines)
  (-let* (((err    . more) (--drop-while (not (flycheck-sml--extract-err it)) lines))
          ((detail more) (--split-with (s-matches? (rx bos (>= 2 space)) it) more)))
    (cons (cons err detail)
          (-when-let (next (--drop-while (s-blank? it) more))
            (-filter 'car (flycheck-sml--grouped-into-errs next))))))

(defun flycheck-sml--extract-errors-from-process-output (checker buf outstr)
  (->> (s-lines outstr)
       (flycheck-sml--grouped-into-errs)
       (--map (s-join "\n" it))
       (--keep (-when-let ((&plist :file file :line line :col col :level level :message msg)
                           (flycheck-sml--extract-err it))
                 (flycheck-error-new-at line col level msg :checker checker :buffer buf :filename file)))))

(defun flycheck-sml--process-start (checker callback)
  (condition-case-unless-debug _
      (let* ((comint-buf (flycheck-sml--run))
             (proc (get-buffer-process comint-buf))
             (buf (current-buffer)))
        (set-process-filter proc
                            (lambda (_proc str)
                              (let ((errs (flycheck-sml--extract-errors-from-process-output checker buf str)))
                                (funcall callback 'finished errs))))

        (sml-prog-proc-send-string proc
                                   (sml-prog-proc--call load-cmd (buffer-file-name buf)))
        (list :comint-buffer comint-buf :process proc))
    (error
     (funcall callback 'errored "Syntax checker failed."))))

(defun flycheck-sml--process-interrupt (_checker context)
  (-when-let ((&plist :process proc :comint-buffer buf) context)
    (kill-process proc)
    (kill-buffer buf)))

(flycheck-define-generic-checker 'sml-nj
  "Source checker using SML/NJ."
  :modes 'sml-mode
  :start #'flycheck-sml--process-start
  :interrupt #'flycheck-sml--process-interrupt
  :predicate (lambda ()
               (and (derived-mode-p 'sml-mode)
                    (not (derived-mode-p 'sml-lex-mode))))
  :error-filter
  (lambda (errors)
    (-> errors
        flycheck-dedent-error-messages
        flycheck-sanitize-errors)))

(add-to-list 'flycheck-checkers 'sml-nj)

(provide 'flycheck-sml)

;;; flycheck-sml.el ends here
