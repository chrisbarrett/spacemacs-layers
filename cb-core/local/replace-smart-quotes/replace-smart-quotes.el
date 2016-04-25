;;; replace-smart-quotes.el --- Functions for replacing smart quotes.  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun replace-smart-quotes-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp (rx (group (or "‘" "’")))
                                  end t)
      (replace-match "'"))))

;;;###autoload
(defun replace-smart-quotes-buffer ()
  (interactive)
  (replace-smart-quotes-region (point-min) (point-max)))

(provide 'replace-smart-quotes)

;;; replace-smart-quotes.el ends here
