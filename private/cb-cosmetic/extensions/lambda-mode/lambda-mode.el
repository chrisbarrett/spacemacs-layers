;;; lambda-mode.el --- Pretty-print lambdas

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This idea is *completely* stolen from Luke Gorrie's pretty-lambda.el.  I'm
;; just curious to see how easily it can be done with font-locking alone.

;;; Code:

(defvar lambda-regex "lambda"
  "A regular expression matching things to convert to lambda symbols.")

(defvar lambda-symbol (string 54091) "The symbol to use for lambdas")

(defun lambda-fontify (beg end)
  (save-excursion
    (lambda-unfontify beg end)
    ;; Mark incorrect uses of spacing.
    (goto-char beg)
    (while (re-search-forward lambda-regex end t)
      (let ((o (car (overlays-at (match-beginning 0)))))
        (unless (and o (eq (overlay-get o 'type) 'lambda))
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'type 'lambda)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'display lambda-symbol)))))))


(defun lambda-unfontify (beg end)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'lambda)
              (delete-overlay o)))
        (overlays-in beg end)))


(define-minor-mode lambda-mode
  "Indicate where only a single space has been used."
  nil " lambda" nil
  (cond ((not lambda-mode)
         (jit-lock-unregister 'lambda-fontify)
         (lambda-unfontify (point-min) (point-max)))
        (t (lambda-fontify (point-min) (point-max))
           (jit-lock-register 'lambda-fontify))))

(provide 'lambda-mode)

;;; lambda-mode.el ends here
