;;; cb-core-rename-file-and-buffer.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1"))

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
(require 'f)
(require 's)

(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'recentf-cleanup "recentf")

(defun cb-core-rename-file-and-buffer--assert-file-exists-for-buffer (&optional buf)
  (let ((cur (buffer-file-name buf)))
    (if (not (and cur (f-exists? cur)))
        (error "Buffer is not visiting a file!")
      cur)))

(defun cb-core-rename-file-and-buffer--try-move-file-with-vc (src dest)
  (condition-case err
      (when (vc-backend src)
        (vc-rename-file src dest)
        t)
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((s-matches? "New file already exists" msg) nil)
        ((s-matches? "Please update files" msg)
         (unless (y-or-n-p "VC cannot track this change automatically.  Continue? ")
           (error msg)))
        (t
         (error msg)))))))

(defun cb-core-rename-file-and-buffer--try-rename-file (src dest)
  (when (and (f-exists? dest) (not (y-or-n-p "File exists.  Overwrite? ")))
    (user-error "Aborted"))
  (rename-file src dest t)
  (-when-let (buf (get-file-buffer src))
    (with-current-buffer buf
      (rename-buffer dest)
      (set-visited-file-name dest)
      (set-buffer-modified-p nil))

    (recentf-cleanup)
    (when (projectile-project-p)
      (projectile-invalidate-cache nil))))

;;;###autoload
(defun cb-core-rename-file-and-buffer (buffer dest-dir dest-filename)
  "Rename the current buffer and file it is visiting.
Performs basic VC cleanup.

BUFFER is the buffer to rename.

DEST-DIR is the directory to move the underlying file to.

DEST-FILENAME is the new filename for the underlying file."
  (interactive (let ((cur (cb-core-rename-file-and-buffer--assert-file-exists-for-buffer)))
                 (list (current-buffer)
                       (read-directory-name "Move to directory: " (f-dirname cur))
                       (read-string "New name: " (f-filename cur)))))
  (let ((src (cb-core-rename-file-and-buffer--assert-file-exists-for-buffer buffer))
        (dest-path (f-join dest-dir dest-filename)))
    (or (cb-core-rename-file-and-buffer--try-move-file-with-vc src dest-path)
        (cb-core-rename-file-and-buffer--try-rename-file src dest-path))
    (when (and (fboundp 'projectile-project-p) (projectile-project-p))
      (call-interactively #'projectile-invalidate-cache))
    (message "File '%s' moved to '%s'" (f-short (f-filename src)) (f-short dest-path))))

;;;###autoload
(defun cb-core-rename-file-and-buffer-mv (buffer to-dir)
  "Move BUFFER's corresponding file to TO-DIR."
  (interactive (list (current-buffer) (read-directory-name "Move to: ")))
  (let ((current-file-name (f-filename (cb-core-rename-file-and-buffer--assert-file-exists-for-buffer buffer))))
    (cb-core-rename-file-and-buffer buffer to-dir current-file-name)))

(provide 'cb-core-rename-file-and-buffer)

;;; cb-core-rename-file-and-buffer.el ends here
