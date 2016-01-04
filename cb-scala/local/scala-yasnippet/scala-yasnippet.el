;;; scala-yasnippet.el --- Helper functions for scala snippets

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

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

;; Helper functions for scala snippets

;;; Code:

(require 's)
(require 'dash)

(eval-when-compile
  (require 'yasnippet nil t))

(defun yas/bol? ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))

(defvar scala-yasnippet--root (file-name-directory load-file-name))

(defun scala-yasnippet-initialise ()
  (add-to-list 'yas-snippet-dirs (concat scala-yasnippet--root "snippets") t))

;;; Slick table mapping template

(defun scala-yasnippet-slick-star-fields (attrs)
  (let ((names (--map (plist-get it :name) (scala-yasnippet--parse-attrs attrs))))
    (s-join ", " names)))

(defun scala-yasnippet-slick-column-defs (attrs)
  (let ((defs (-map 'scala-yasnippet--slick-attr-to-def (scala-yasnippet--parse-attrs attrs)))
        (indent (current-indentation)))
    (s-join (concat "\n" (s-repeat indent " ")) defs)))

(defun scala-yasnippet--slick-attr-to-def (attr)
  (-let [(&plist :name name :type type) attr]
    (format "def %s = column[%s](\"%s\")" name type name)))

(defun scala-yasnippet--parse-attrs (attrs)
  (let ((ctor-args (s-split (rx (* space) "," (* space)) attrs)))
    (--map (-let [(_ name type) (s-match (rx (group (*? nonl))
                                             (* space) ":" (* space)
                                             (group (* nonl)))
                                         (s-trim it))]
             (list :name (or name "x") :type (or type "T")))
           ctor-args)))

;;; Test fixtures

(defun scala-yasnippet-test-fixture-name ()
  (or (ignore-errors (s-replace "Tests" "Test" (f-filename (f-no-ext (buffer-file-name)))))
      "TestFixture"))

(provide 'scala-yasnippet)

;;; scala-yasnippet.el ends here
