;;; light-workgroups.el --- Light-weight workgroups implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Light-weight workgroups implementation

;;; Code:

(require 's)
(require 'dash)

(defconst moviomedia-dir "~/Movio/moviomedia")

(defun light-workgroups--interp (spec)
  "Evaluate the given workgroups SPEC."
  (pcase spec
    (`(:frame ,name . ,windows)
     (let ((frame (if (equal name "primary") (selected-frame) (make-frame))))
       (select-frame frame)
       (set-frame-name name))

     (light-workgroups--interp windows))

    (`(:window ,n ,fn)
     (let ((buf (save-window-excursion (funcall fn))))
       (if (equal n 1)
           (switch-to-buffer buf)
         (split-window-horizontally)
         (other-window 1)
         (switch-to-buffer buf))))

    ((and `(,h . _) (guard (keywordp h)))
     (error "Unable to interpret keyword '%s' in %s" h spec))

    ((pred listp)
     (-each spec 'light-workgroups--interp))

    (x
     (error "Unable to interpret form: %s" x))))

(defun light-workgroups--pre-process (spec)
  (delete-other-frames)
  (delete-other-windows)
  spec)



(defun mm ()
  "Prepare frames for moviomedia."
  (interactive)
  (light-workgroups--interp
   (light-workgroups--pre-process
    '((:frame "primary"
              (:window 1 (lambda ()
                           (org-agenda nil "w")
                           (--first (with-current-buffer it (derived-mode-p 'org-agenda-mode))
                                    (buffer-list))))
              (:window 2 (lambda ()
                           (let* ((default-directory moviomedia-dir)
                                  (buf (sbt-start)))
                             (with-current-buffer buf
                               (goto-char (point-max))
                               (evil-insert-state))
                             buf))))
      (:frame "secondary"
              (:window 1 (lambda ()
                           (magit-status moviomedia-dir)
                           (current-buffer))))))))

(provide 'light-workgroups)

;;; light-workgroups.el ends here
