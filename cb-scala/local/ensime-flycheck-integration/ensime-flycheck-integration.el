;;; ensime-flycheck-integration.el --- Provide a uniform interface for combining ensime and flycheck.  -*- lexical-binding: t; -*-

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

(require 'ensime nil t)

(autoload 'evil-define-key "evil-core")
(autoload 'flycheck-next-error "flycheck")
(autoload 'flycheck-next-error-pos "flycheck")

(defun ensime-flycheck-integration--next-ensime-note ()
  (-let* ((start (point))
          (conn (ensime-connection))
          (notes (append (ensime-java-compiler-notes conn)
                         (ensime-scala-compiler-notes conn)))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes t))
          (end (if beg (1+ beg) (point-min))))
    (when (< start end)
      end)))

(defun ensime-flycheck-integration--prev-ensime-note ()
  (-let* ((start (point))
          (conn (ensime-connection))
          (notes (append (ensime-java-compiler-notes conn)
                         (ensime-scala-compiler-notes conn)))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes nil))
          (end (if beg (1+ beg) (point-max))))
    (when (> start end)
      end)))

(defun ensime-flycheck-integration--next-flycheck-note ()
  (flycheck-next-error-pos 1))

(defun ensime-flycheck-integration--prev-flycheck-note ()
  (flycheck-next-error-pos -1))

(defun ensime-flycheck-integration-next-error ()
  "Move forward to the closest Flycheck or ENSIME error."
  (interactive)
  (let* ((flycheck-pos (ensime-flycheck-integration--next-flycheck-note))
         (ensime-pos (ensime-flycheck-integration--next-ensime-note)))

    (call-interactively
     (cond
      ((and (null flycheck-pos) (null ensime-pos))
       (user-error "No more errors"))

      ((null flycheck-pos) #'ensime-forward-note)
      ((null ensime-pos) #'flycheck-next-error)
      ((< flycheck-pos ensime-pos) #'flycheck-next-error)
      (t
       #'ensime-forward-note)))))

(defun ensime-flycheck-integration-prev-error ()
  "Move backward to the closest Flycheck or ENSIME error."
  (interactive)
  (let* ((flycheck-pos (ensime-flycheck-integration--prev-flycheck-note))
         (ensime-pos (ensime-flycheck-integration--prev-ensime-note)))
    (call-interactively
     (cond
      ((and (null flycheck-pos) (null ensime-pos))
       (user-error "No more errors"))

      ((null flycheck-pos) #'ensime-backward-note)
      ((null ensime-pos) #'flycheck-previous-error)
      ((< flycheck-pos ensime-pos) #'ensime-backward-note)
      (t
       #'flycheck-previous-error)))))

(defun ensime-flycheck-integration-init ()
  (with-eval-after-load 'ensime
    (define-key ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (define-key ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error))
  (with-eval-after-load 'evil-core
    (evil-define-key 'normal ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)))

(provide 'ensime-flycheck-integration)

;;; ensime-flycheck-integration.el ends here
