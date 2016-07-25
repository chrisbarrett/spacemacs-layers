;;; cb-org-subtree.el --- Utilities for working with subtrees in org buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((f "0.17.2") (org "8.3.4"))

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

(require 'f)
(require 'org)

;;;###autoload
(defun cb-org-subtree-narrow-to-content ()
  "Narrow to the content of the subtree.  Excludes the heading line."
  (widen)
  (unless (org-at-heading-p) (org-back-to-heading))
  (org-narrow-to-subtree)
  (forward-line)
  (narrow-to-region (line-beginning-position) (point-max)))

;;;###autoload
(defun cb-org-subtree-content ()
  "Return the content of the subtree at point as a string."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun cb-org-subtree-write-content (dest)
  "Write the contents of the subtree at point to a file at DEST."
  (interactive (list (ido-read-file-name "Write subtree to: " nil nil nil ".org")))
  (f-write-text (cb-org-subtree-content) 'utf-8 dest)
  (when (called-interactively-p nil)
    (message "Subtree written to %s" dest)))

;;;###autoload
(defun cb-org-subtree-copy ()
  "Create a duplicate of the current subtree at the given heading."
  (interactive "*")
  (atomic-change-group
    (org-copy-subtree)
    (org-clone-subtree-with-time-shift 1 '(16))
    (call-interactively 'org-refile)))

(provide 'cb-org-subtree)

;;; cb-org-subtree.el ends here
