;;; insert-shebang.el --- Command for inserting shebang lines  -*- lexical-binding: t; -*-

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

(defgroup insert-shebang nil
  "Command for inserting shebang lines."
  :group 'editing
  :prefix "insert-shebang-")

(defcustom insert-shebang-interpreter-alist
  '(("el" . "emacs")
    ("hs" . "runhaskell")
    ("py" . "python")
    ("rb" . "ruby")
    ("sh" . "bash")
    ("zsh" . "zsh"))
  "Alist of file extensions to interpreter commands."
  :group 'insert-shebang
  :type '(alist :key-type string :value-type string))

(defun insert-shebang--interpreter-for-file (filename)
  (cdr (assoc (file-name-extension filename) insert-shebang-interpreter-alist)))

;;;###autoload
(defun insert-shebang (cmd)
  "Insert a shebang line at the top of the current buffer.
Prompt for a command CMD if one cannot be guessed."
  (interactive
   (list (or (insert-shebang--interpreter-for-file (buffer-file-name))
             (read-string "Command name: " nil t))))
  (save-excursion
    (goto-char (point-min))
    (open-line 2)
    (insert (concat "#!/usr/bin/env " cmd))))

(provide 'insert-shebang)

;;; insert-shebang.el ends here
