;;; cb-rotate-window-split.el --- Swap between vertical and horizontal split. -*- lexical-binding: t; -*-

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

(defun cb-rotate-window-split--rotate-layout ()
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                                     (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
                 (car (window-edges (next-window))))
              'split-window-horizontally
            'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (when this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (when this-win-2nd (other-window 1)))))

;;;###autoload
(defun cb-rotate-window-split ()
  "Swap between vertical and horizontal split."
  (interactive)
  (cond
   ((= (count-windows) 2)
    (cb-rotate-window-split--rotate-layout))
   ((= 1 (count-windows))
    (user-error "No windows to rotate"))
   (t
    (user-error "Too many windows to rotate"))))

(provide 'cb-rotate-window-split)

;;; cb-rotate-window-split.el ends here
