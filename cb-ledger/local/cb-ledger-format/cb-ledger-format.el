;;; cb-ledger-format.el --- Buffer formatting commands for ledger files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (ledger-mode "20160228.1734"))

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

(require 'ledger-mode)
(require 's)

(defun cb-ledger-format--align-price-assertion ()
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (when (s-matches? (rx "=" (* space) (not (any digit)))
                      line)
      (unwind-protect
          (progn
            (goto-char (line-beginning-position))
            (search-forward "=")
            (goto-char (match-beginning 0))
            (indent-to (1+ ledger-post-amount-alignment-column))
            (skip-chars-forward " =")
            (just-one-space))
        (goto-char (line-end-position))))))

;;;###autoload
(defun cb-ledger-format-buffer ()
  "Reformat the buffer."
  (interactive "*")
  (let ((pos (point)))
    (ledger-mode-clean-buffer)
    (goto-char (point-min))
    (while (search-forward "=" nil t)
      (cb-ledger-format--align-price-assertion))
    (goto-char pos)))

(provide 'cb-ledger-format)

;;; cb-ledger-format.el ends here
