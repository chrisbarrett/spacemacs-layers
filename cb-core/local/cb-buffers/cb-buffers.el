;;; cb-buffers.el --- Buffer utilities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((s "1.10.0") (dash "2.12.1"))
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

;; Utilities and commands for buffer interaction.

;;; Code:

(require 'dash)
(require 's)

(defgroup cb-buffers nil
  "Functions for working with buffers."
  :group 'editing
  :prefix "cb-buffers-")

(defcustom cb-buffers-kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*" "*elfeed-search*"
    "work_movio.org" "diary.org" "notes.org" "*spacemacs*" " *mu4e-main*")
  "List of buffers to always bury instead of killing."
  :group 'cb-buffers
  :type '(list string))

(defcustom cb-buffers-kill-buffer-if-no-proc-list
  '("*shell*" "*eshell*" "*ansi-term*")
  "List of buffers to always kill if they do not have an associated process."
  :group 'cb-buffers
  :type '(list string))

(defcustom cb-buffers-kill-buffer-ignored-modes
  '(circe-mode)
  "List of modes whose buffers should be buried and not killed."
  :group 'cb-buffers
  :type '(list symbol))

(defcustom cb-buffers-indent-commands-alist
  nil
  "Alist of commands to run to indent the buffer, indexed by major mode."
  :group 'cb-buffers
  :type '(alist :key-type symbol :value-type function))

(defcustom cb-buffers-lisp-modes
  '(cider-repl-mode
    clojure-mode
    clojurescript-mode
    common-lisp-mode
    emacs-lisp-mode
    geiser-repl-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-mode
    repl-mode
    scheme-mode
    slime-mode
    slime-repl-mode
    extempore-mode
    inferior-extempore-mode)
  "List of major modes that use Lisp editing conventions."
  :group 'cb-buffers
  :type '(list symbol))


(defcustom cb-buffers-prompt-modes
  '(comint-mode
    inf-ruby-mode
    inferior-python-mode
    ielm-mode
    erc-mode
    term-mode
    utop-mode
    slime-repl-mode
    inferior-scheme-mode
    inferior-haskell-mode
    sclang-post-buffer-mode)
  "List of major modes that use prompt editing conventions."
  :group 'cb-buffers
  :type '(list symbol))

(cl-defmacro cb-buffers-filtera (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric buffer filtering macro.

Filter over the buffers, calling PRED-FORM with each buffer
set to current.

Filters over BUFS, or the buffer list if unspecified."
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(defun cb-buffers-maybe-kill ()
  "Kill or bury the current buffer."
  (interactive)
  (if (cb-buffers--buffer-ignored-or-live? (current-buffer))
      (bury-buffer)
    ;; HACK: Avoid read-only text property errors.
    (let ((inhibit-read-only t))
      (kill-buffer (current-buffer)))))

(defun cb-buffers-maybe-kill-all ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (let ((other-buffers (-difference (buffer-list) (list (current-buffer)))))
    (dolist (buf other-buffers)
      (with-current-buffer buf
        (cb-buffers-maybe-kill)))))

(defun cb-buffers--buffer-ignored-or-live? (buf)
  (or (-contains? (-concat cb-buffers-kill-buffer-if-no-proc-list
                           cb-buffers-kill-buffer-ignored-list)
                  (buffer-name buf))
      (apply #'derived-mode-p cb-buffers-kill-buffer-ignored-modes)
      (-when-let (proc (get-buffer-process buf))
        (process-live-p proc))))

(defun cb-buffers-current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun cb-buffers-indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (s-blank? (cb-buffers-current-line))
          (indent-according-to-mode))
        (forward-line)))))

(defun cb-buffers-indent-dwim (&optional arg)
  "Perform a context-sensitive indentation action.
With prefix argument ARG, justify text."
  (interactive "P")
  (let ((in-string? (nth 8 (syntax-ppss))))
    (cond
     ((region-active-p)
      (indent-region (region-beginning) (region-end))
      (message "Indented region."))

     (in-string?
      (if (apply 'derived-mode-p cb-buffers-lisp-modes)
          (lisp-fill-paragraph arg)
        (or (fill-comment-paragraph)
            (fill-paragraph arg)))
      (message "Filled paragraph."))

     ((assoc major-mode cb-buffers-indent-commands-alist)
      (funcall (cdr (assoc major-mode cb-buffers-indent-commands-alist)))
      (message "Formatted buffer."))

     (t
      (cb-buffers-indent-whole-buffer)
      (message "Indented buffer.")))))

(defun cb-buffers-outdent-line ()
  "Remove indentation on the current line."
  (interactive "*")
  (save-excursion
    (goto-char (line-beginning-position))
    (delete-horizontal-space)))

(defun cb-buffers-in-string-or-comment? ()
  "Non-nil if point is at a string or comment."
  (or (nth 8 (syntax-ppss))
      (-intersection (list font-lock-comment-face font-lock-doc-face font-lock-string-face)
                     (face-at-point nil t))))

(provide 'cb-buffers)

;;; cb-buffers.el ends here
