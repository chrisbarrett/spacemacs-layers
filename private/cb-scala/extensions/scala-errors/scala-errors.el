;;; scala-errors.el --- Quickly navigate to errors in a scala project

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (f "0.16.0") (dash "2.5.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Quickly navigate to errors in a scala project. Requires the 'sbt-quickfix'
;; SBT plugin:
;;
;;   https://github.com/dscleaver/sbt-quickfix

;;; Code:


(require 'f)
(require 's)
(require 'dash)
(require 'rx)

(defvar-local scala-errors--quickfix-last-update nil
  "Stores the last time the quickfix file was updated.
This is set after comparisons with the current mod date of the file on disk.")


;;;###autoload
(defun scala-errors-show-errors ()
  "Display SBT errors in a compilation buffer."
  (interactive)
  (scala-errors--prepare-error-buffer)
  (display-buffer (scala-errors--error-buffer)))

;;;###autoload
(defun scala-errors-goto-next-error ()
  "Navigate to the next SBT error."
  (interactive)
  (save-window-excursion
    (scala-errors-show-errors)
    (call-interactively 'next-error)))

;;;###autoload
(defun scala-errors-goto-prev-error ()
  "Navigate to the previous SBT error."
  (interactive)
  (save-window-excursion
    (scala-errors-show-errors)
    (call-interactively 'previous-error)))


(defun scala-errors--prepare-error-buffer ()
  "Load the quickfix file into a compilation buffer."
  (let ((qf-file (scala-errors--quickfix-file-path))
        (updated? (or (null scala-errors--quickfix-last-update) (scala-errors--quickfix-file-updated?))))
    (setq scala-errors--quickfix-last-update (scala-errors--quickfix-file-modified-time))

    (with-current-buffer (scala-errors--error-buffer)
      (when (or updated? (s-blank? (buffer-string)))
        (message "Updating errors list")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "SBT errors\n\n" 'font-lock-face 'font-lock-comment-face))
          (insert (scala-errors--process-quickfix-file-contents (f-read-text qf-file)))
          (goto-char (point-min)))

        (compilation-mode "SBT Compilation")))))

(defun scala-errors--quickfix-file-updated? ()
  "Non-nil if the quickfix file has been updated since the error buffer was last displayed."
  (time-less-p scala-errors--quickfix-last-update (scala-errors--quickfix-file-modified-time)))

(defun scala-errors--quickfix-file-modified-time ()
  "The modification time of the quickfix file."
  (elt (file-attributes (scala-errors--quickfix-file-path)) 5))

(defun scala-errors--error-buffer ()
  "Return the buffer to use for displaying SBT errors."
  (let* ((proj (scala-errors--find-project-root))
         (bufname (format "*sbt errors<%s>*" proj)))
    (get-buffer-create bufname)))

(defun scala-errors--find-project-root ()
  "Search upwards for the project root."
  (or (locate-dominating-file default-directory "target")
      (locate-dominating-file default-directory "build.sbt")
      (locate-dominating-file default-directory "src")))

(defun scala-errors--process-quickfix-file-contents (str)
  "Prettify the STR for display in the error buffer."
  (->> str
       (replace-regexp-in-string (rx bol "[" (or "error" "warn") "]" (* space)) "")
       (s-lines)
       (--remove (s-contains? "Cannot run program \"gvim\": error=2, No such file or directory" it))
       (s-join "\n")))

(defun scala-errors--quickfix-file-path ()
  "Search upwards for the path to the quickfix file."
  (or (-when-let* ((proj-root (locate-dominating-file default-directory "target"))
                   (file (f-join proj-root "target/quickfix/sbt.quickfix")))
        (when (f-exists? file)
          file))
      (error "No error file found")))


;;; Evil key bindings

(eval-after-load 'evil-leader
  '(progn
     (evil-leader/set-key-for-mode 'scala-mode "mff" 'scala-errors-show-errors)
     (evil-leader/set-key-for-mode 'scala-mode "mfn" 'scala-errors-goto-next-error)
     (evil-leader/set-key-for-mode 'scala-mode "mfp" 'scala-errors-goto-prev-error)))


(provide 'scala-errors)

;;; scala-errors.el ends here
