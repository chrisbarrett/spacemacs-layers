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
(require 'compile)

(defvar-local scala-errors--quickfix-last-update nil
  "Stores the last time the quickfix file was updated.
This is set after comparisons with the current mod date of the file on disk.")

(defvar scala-errors--file-polling-interval 0.2
  "The interval in seconds at which to poll for quickfix file creation.
Used when refreshing the error list.")

(defvar scala-errors--file-polling-max-time 5
  "The maximum time in seconds for polling for the quickfix file.
Used when refreshing the error list.")


;;;###autoload
(defun scala-errors-show-errors ()
  "Display SBT errors in a compilation buffer."
  (interactive)
  (let ((buf (find-file-noselect (scala-errors--quickfix-file-path) t)))
    (with-current-buffer buf
      (rename-buffer (scala-errors--buffer-name))
      (cond
       ((not (f-exists? (buffer-file-name)))
        (scala-errors-refresh))
       (t
        (scala-errors-mode)
        (goto-char (point-min))
        (pop-to-buffer buf)
        (resize-temp-buffer-window (get-buffer-window buf))
        (message "Press 'g' to refresh errors if they get out-of-sync with SBT"))))
    buf))

;;;###autoload
(defun scala-errors-goto-first-error ()
  "Navigate to the next SBT error."
  (interactive)
  (let ((buf (scala-errors-show-errors)))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (goto-char (point-min))
          (compile-goto-error))
      (user-error "No errors"))))

;;;###autoload
(defun scala-errors-goto-next-error ()
  "Navigate to the next SBT error."
  (interactive)
  (save-window-excursion (scala-errors-show-errors))
  (call-interactively 'next-error))

;;;###autoload
(defun scala-errors-goto-prev-error ()
  "Navigate to the previous SBT error."
  (interactive)
  (save-window-excursion (scala-errors-show-errors))
  (call-interactively 'previous-error))

;;;###autoload
(defun scala-errors-refresh ()
  "Delete the quickfix file and redisplay the errors list."
  (interactive)
  (scala-errors--delete-quickfix)
  (scala-errors--force-generate-quickfix)
  (run-with-timer scala-errors--file-polling-interval nil
                  'scala-errors--poll-until-exists 0)
  (message "Refreshing SBT errors..."))

(defun scala-errors--poll-until-exists (repetitions)
  (let ((time-spent (* scala-errors--file-polling-interval repetitions)))
    (cond
     ((< scala-errors--file-polling-max-time time-spent)
      (message (format "No error output within %s seconds" scala-errors--file-polling-max-time))
      (message "No errors from SBT"))
     ((f-exists? (scala-errors--quickfix-file-path))
      (scala-errors-show-errors))
     (t
      (run-with-timer scala-errors--file-polling-interval
                      nil 'scala-errors--poll-until-exists (1+ repetitions))))))

(defun scala-errors--delete-quickfix ()
  (-when-let* ((buf (get-buffer (scala-errors--buffer-name)))
               (file (buffer-file-name buf)))
    (when (f-exists? file) (f-delete file))

    (-when-let (wins (get-buffer-window-list buf))
      (-each wins 'delete-window))

    (kill-buffer buf)))

(defun scala-errors--force-generate-quickfix ()
  (-when-let (last-scala-buf
              (--first (with-current-buffer it (derived-mode-p 'scala-mode))
                       (buffer-list)))
    (with-current-buffer last-scala-buf
      (scala-errors--touch))))

(defun scala-errors--touch ()
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(defun scala-errors--buffer-name ()
  (format "*SBT errors<%s>*" (scala-errors--project-name)))


(defun scala-errors--project-root ()
  (or (locate-dominating-file default-directory "target")
      (locate-dominating-file default-directory "build.sbt")
      (locate-dominating-file default-directory "src")
      (locate-dominating-file default-directory ".git")))

(defun scala-errors--project-name ()
  (-last-item (f-split (scala-errors--project-root))))

(defun scala-errors--goto-first-compilation-link ()
  (goto-char (point-min))
  (while (not (or (eobp)
                  (get-text-property (point) 'compilation-message)))
    (forward-char 1)))

(defun scala-errors--quickfix-file-path ()
  "Search upwards for the path to the quickfix file."
  (-when-let (proj-root (scala-errors--project-root))
    (f-join proj-root "target/quickfix/sbt.quickfix")))


(define-compilation-mode scala-errors-mode "Scala Errors"
  "Compilation mode for SBT compiler errors using the QuickFix SBT plugin."
  (auto-revert-mode +1)
  (read-only-mode +1)

  ;; Disable recompile
  (local-set-key (kbd "g") nil)
  (local-set-key (kbd "g") 'scala-errors-refresh)

  (font-lock-add-keywords
   nil
   `(
     ;; Hide [error] level notes.
     (,(rx bol "[error]" (? space)) (0 '(face nil invisible t)))
     ;; Hide gvim call
     (,(rx bol (* nonl) "Cannot run program \"gvim\"" (* nonl) eol) (0 '(face nil invisible t)))
     ;; Highlight error column arrows
     (,(rx bol  (* space) (or "[error]" "[warn]") (+ space) (group "^") (* space) eol)
      (1 font-lock-constant-face))
     ;; Highlight error column arrows
     (,(rx bol  (* space) (or "[error]" "warn")
           (+ space) (group (+ alpha) space "error" (? "s") space "found") (* space) eol)
      (1 font-lock-comment-face))

     (,
      (rx (group "/" (+ (not (any ":"))) "/") (+ (not (any "/" ":"))) ":" (+ num) ":" (group (* nonl)))
      ;; Shorten paths
      (1 '(face nil invisible t))
      ;; Colour messages
      (2 font-lock-string-face)))))


(eval-after-load 'aggressive-indent
  '(add-to-list 'aggressive-indent-excluded-modes 'scala-errors-mode))


;;; Evil key bindings

(eval-after-load 'evil-leader
  '(progn
     (evil-leader/set-key-for-mode 'scala-mode "mfl" 'scala-errors-show-errors)
     (evil-leader/set-key-for-mode 'scala-mode "mfg" 'scala-errors-refresh)
     (evil-leader/set-key-for-mode 'scala-mode "mff" 'scala-errors-goto-first-error)
     (evil-leader/set-key-for-mode 'scala-mode "mfn" 'scala-errors-goto-next-error)
     (evil-leader/set-key-for-mode 'scala-mode "mfp" 'scala-errors-goto-prev-error)))


(provide 'scala-errors)

;;; scala-errors.el ends here
