;;; indent-dwim.el --- Provides a generalised indentation command. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0"))

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

;; Provides a generalised indentation command.

;;; Code:

(require 's)
(autoload 'evil-define-key "evil-core")

(defgroup indent-dwim nil
  "Provides a generalised indentation command."
  :group 'editing
  :prefix "indent-dwim-")

(defcustom indent-dwim-commands-alist
  nil
  "Alist of commands to run to indent the buffer, indexed by major mode."
  :group 'indent-dwim
  :type '(alist :key-type symbol :value-type function))

(defcustom indent-dwim-lisp-modes
  (if (boundp 'cb-vars-lisp-modes)
      cb-vars-lisp-modes
    '(clojure-mode
      clojurescript-mode
      common-lisp-mode
      emacs-lisp-mode
      inferior-emacs-lisp-mode
      lisp-mode
      scheme-mode))
  "List of major modes that use Lisp editing conventions."
  :group 'indent-dwim
  :type '(list symbol))

(defun indent-dwim-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (unless (s-blank? line)
            (indent-according-to-mode)))
        (forward-line)))))

;;;###autoload
(defun indent-dwim (&optional arg)
  "Perform a context-sensitive indentation action.
With prefix argument ARG, justify text."
  (interactive "P")
  (let ((in-string? (nth 8 (syntax-ppss))))
    (cond
     ((region-active-p)
      (indent-region (region-beginning) (region-end))
      (message "Indented region."))

     (in-string?
      (if (apply 'derived-mode-p indent-dwim-lisp-modes)
          (lisp-fill-paragraph arg)
        (or (fill-comment-paragraph)
            (fill-paragraph arg)))
      (message "Filled paragraph."))

     ((assoc major-mode indent-dwim-commands-alist)
      (funcall (cdr (assoc major-mode indent-dwim-commands-alist)))
      (message "Formatted buffer."))

     (t
      (indent-dwim-whole-buffer)
      (message "Indented buffer.")))))

;;;###autoload
(defun indent-dwim-init ()
  (define-key prog-mode-map (kbd "M-q") #'indent-dwim)
  (evil-define-key 'normal  prog-mode-map (kbd "M-q") #'indent-dwim)

  (with-eval-after-load 'sgml-mode
    (evil-define-key 'normal  (with-no-warnings sgml-mode-map)
      (kbd "M-q") #'indent-dwim))

  (with-eval-after-load 'nxml-mode
    (evil-define-key 'normal (with-no-warnings nxml-mode-map)
      (kbd "M-q") #'indent-dwim)))

(provide 'indent-dwim)

;;; indent-dwim.el ends here
