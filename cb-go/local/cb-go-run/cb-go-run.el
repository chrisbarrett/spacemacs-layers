;;; cb-go-run.el --- Run commands for go  -*- lexical-binding: t; -*-

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

;;; Code:

(defgroup cb-go-run nil
  "Commands for interacting with tests in go."
  :group 'languages
  :prefix "cb-go-run-")

(defcustom cb-go-run-use-gocheck? t
  "Whether to use gocheck. If nil, fall back to `-run`."
  :group 'cb-go-run
  :type 'boolean)

;;;###autoload
(defun cb-go-run-tests (names)
  "Run all unit tests with NAMES."
  (interactive)
  (save-selected-window
    (async-shell-command (concat "go test " names))))

;;;###autoload
(defun cb-go-run-package-tests ()
  "Run all tests in the package."
  (interactive)
  (cb-go-run-tests ""))

;;;###autoload
(defun cb-go-run-package-tests-nested ()
  "Run all tests in this package and its enclosing packages."
  (interactive)
  (cb-go-run-tests "./..."))

;;;###autoload
(defun cb-go-run-test-current-function ()
  "Run tests for the current function."
  (interactive)
  (unless (string-match "_test\\.go" buffer-file-name)
    (user-error "Must be in a _test.go file"))
  (save-excursion
    (let ((test-method (if cb-go-run-use-gocheck? "-check.f" "-run")))
      (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
      (cb-go-run-tests (format "%s='%s'" test-method (match-string 2))))))

;;;###autoload
(defun cb-go-run-test-current-suite ()
  "Run current test suite."
  (interactive)
  (unless (string-match "_test\.go" buffer-file-name)
    (user-error "Must be in a _test.go file to run go-test-current-suite"))
  (unless cb-go-run-use-gocheck?
    (user-error "Gocheck is needed to test the current suite"))
  (save-excursion
    (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
    (cb-go-run-tests (format "-check.f='%s'" (match-string 2)))))

;;;###autoload
(defun cb-go-run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (shell-command (format "go run %s" (shell-quote-argument (buffer-file-name)))))

(provide 'cb-go-run)

;;; cb-go-run.el ends here
