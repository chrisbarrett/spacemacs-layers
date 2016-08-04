;;; cb-scala-eldoc.el --- Show eldoc in ensime.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

(autoload 'ensime-connected-p "ensime-client")
(autoload 'ensime-print-type-at-point "ensime-inspector")

(defun cb-scala-eldoc--enable-eldoc ()
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode +1))

;;;###autoload
(defun cb-scala-eldoc-init ()
  (add-hook 'ensime-mode-hook #'cb-scala-eldoc--enable-eldoc))

(provide 'cb-scala-eldoc)

;;; cb-scala-eldoc.el ends here
