;;; indirect-region.el --- Create indirect buffer from region.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((dash "2.12.1"))

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

;; Provides a command for creating an indirect buffer from a region.

;;; Code:

(require 'dash)

(defvar-local indirect-region--previous-selection nil
  "Mode to set for indirect buffers.")

;;;###autoload
(defun indirect-region (start end mode)
  "Edit the current region in another buffer.

Edit from START to END using MODE."
  (interactive
   (list (region-beginning)
         (region-end)
         (intern (completing-read
                  "Mode: "
                  (--map (list (symbol-name it))
                         (apropos-internal "-mode$" 'commandp))
                  nil t indirect-region--previous-selection))))

  (setq indirect-region--previous-selection (symbol-name mode))
  (let ((buffer-name (generate-new-buffer-name "*indirect*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(provide 'indirect-region)

;;; indirect-region.el ends here
