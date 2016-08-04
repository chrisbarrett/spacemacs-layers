;;; cb-scala-flyspell.el --- Avoid flyspell false positives.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0"))

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

(require 'flyspell)
(require 's)

(defun cb-scala-flyspell--flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package")
                        (buffer-substring (line-beginning-position)
                                          (line-end-position))))))

(defun cb-scala-flyspell--configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate #'cb-scala-flyspell--flyspell-verify))

;;;###autoload
(defun cb-scala-flyspell-init ()
  (add-hook 'scala-mode-hook #'cb-scala-flyspell--configure-flyspell))

(provide 'cb-scala-flyspell)

;;; cb-scala-flyspell.el ends here
