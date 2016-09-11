;;; haskell-pragmas.el --- Utilities for working with Haskell language pragmas.  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 's)

(defun haskell-pragmas-language-pragmas ()
  (s-split "\n" (s-trim (shell-command-to-string "stack ghc -- --supported-languages"))))

(defun haskell-pragmas-in-buffer-string (s ps)
  (--filter (s-matches? (rx-to-string `(and "{-# LANGUAGE" (+ space) (* nonl) ,it)) s)
            ps))

(defun haskell-pragmas--available-language-pragmas ()
  (let ((ps (haskell-pragmas-language-pragmas)))
    (-difference ps (haskell-pragmas-in-buffer-string (buffer-string) ps))))

(defun haskell-pragmas--goto-buffer-start ()
  (goto-char (point-min))

  ;; Skip #! line
  (when (and (s-matches? (rx bol "#!")
                         (buffer-substring (line-beginning-position) (line-end-position)))
             (search-forward "#!" nil t))
    (goto-char (line-end-position))
    (forward-char 1))

  (while (and (not (eobp))
              (s-blank? (buffer-substring (line-beginning-position) (line-end-position))))
    (forward-line 1)))

;;;###autoload
(defun haskell-pragmas-insert (pragma)
  "Read a language PRAGMA to be inserted at the start of this file."
  (interactive (list (completing-read "Pragma: " (haskell-pragmas--available-language-pragmas) nil t)))
  (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
    (save-excursion
      (haskell-pragmas--goto-buffer-start)
      (insert s))))

(provide 'haskell-pragmas)

;;; haskell-pragmas.el ends here
