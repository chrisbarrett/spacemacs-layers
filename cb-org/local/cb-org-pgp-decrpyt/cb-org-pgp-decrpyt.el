;;; cb-org-pgp-decrpyt.el --- Decrypt with C-c C-c.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((org "8.3.4"))

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

(require 'org)
(require 'org-crypt)

(defconst cb-org-pgp-decrpyt--pgp-header "-----BEGIN PGP MESSAGE-----")

(defun cb-org-pgp-decrpyt--looking-at-pgp-section? ()
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (forward-line)
      (looking-at cb-org-pgp-decrpyt--pgp-header))))

(defun cb-org-pgp-decrpyt-decrypt-entry ()
  (when (cb-org-pgp-decrpyt--looking-at-pgp-section?)
    (org-decrypt-entry)
    t))

(defun cb-org-pgp-decrpyt-init ()
  (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-pgp-decrpyt-decrypt-entry))

(provide 'cb-org-pgp-decrpyt)

;;; cb-org-pgp-decrpyt.el ends here
