;;; merlin-eldoc.el --- Eldoc support for Merlin  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

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

(require 'dash)
(require 'merlin)
(require 'noflet)
(require 's)

(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")

(defun merlin-eldoc--type-display (_bounds type &optional quiet)
  (if (not type)
      (unless quiet (message "<no information>"))
    (let ((count 0)
          (pos   0))
      (merlin/display-in-type-buffer type)
      (while (and (<= count 8)
                  (string-match "\n" type pos))
        (setq pos (match-end 0))
        (setq count (1+ count)))
      (with-current-buffer merlin-type-buffer-name
        (font-lock-fontify-region (point-min) (point-max))
        (buffer-string)))))

(defun merlin-eldoc/eldoc-function ()
  (unless (let ((at-open? (rx bol (* space) "open" eow)))
            (or (cb-buffers-in-string-or-comment?)
                (save-excursion
                  (skip-chars-backward "\n \t")
                  (s-matches? at-open? (buffer-substring (line-beginning-position) (line-end-position))))))
    (merlin-sync-to-point)
    (when (merlin--type-enclosing-query)
      (-when-let (res (noflet ((merlin--type-display
                                (bounds type &optional quiet)
                                (merlin-eldoc--type-display bounds type quiet)))
                        (merlin-type-enclosing-go-up)))
        (s-trim res)))))

(defun merlin-eldoc/setup ()
  (eldoc-mode +1)
  (setq-local eldoc-documentation-function #'merlin-eldoc/eldoc-function))

(add-hook 'merlin-mode-hook #'merlin-eldoc/setup)

(provide 'merlin-eldoc)

;;; merlin-eldoc.el ends here
