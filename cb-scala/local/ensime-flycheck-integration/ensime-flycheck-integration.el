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
(require 'flycheck nil t)
(require 'flycheck-pos-tip nil t)

(autoload 'evil-define-key "evil-core")

(defun ensime-flycheck-integration--ensime-buffer-notes ()
  (when (ensime-connected-p)
    (let ((conn (ensime-connection)))
      (append (ensime-java-compiler-notes conn)
              (ensime-scala-compiler-notes conn)))))

(defun ensime-flycheck-integration--next-ensime-note ()
  (-let* ((start (point))
          (notes (ensime-flycheck-integration--ensime-buffer-notes))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes t))
          (end (if beg (ensime-internalize-offset beg) (point-min))))
    (when (< start end)
      end)))

(defun ensime-flycheck-integration--prev-ensime-note ()
  (-let* ((start (point))
          (notes (ensime-flycheck-integration--ensime-buffer-notes))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes nil))
          (end (if beg (ensime-internalize-offset beg) (point-max))))
    (when (> start end)
      end)))

(defun ensime-flycheck-integration--next-flycheck-note ()
  (flycheck-next-error-pos 1))

(defun ensime-flycheck-integration--prev-flycheck-note ()
  (flycheck-next-error-pos -1))

(defun ensime-flycheck-integration--ensime-notes-at-point ()
  (-filter (-lambda ((&plist :beg beg :end end))
             (<= beg (point) (1+ end)))
           (ensime-flycheck-integration--ensime-buffer-notes)))

(defun ensime-flycheck-integration--maybe-display-note-popup ()
  (-when-let (errors
              (-map (-lambda ((&plist :msg msg :severity level :line line :col col))
                      (flycheck-error-new
                       :id ""
                       :buffer (current-buffer)
                       :checker nil
                       :filename (buffer-file-name)
                       :line line
                       :column col
                       :message msg
                       :level level))
                    (ensime-flycheck-integration--ensime-notes-at-point)))
    (if (display-graphic-p)
        (let ((msg (mapconcat #'flycheck-error-format-message-and-id errors "\n\n"))
              (line-height (-when-let ((height . _) (window-line-height))
                             (+ height 5))))
          (pos-tip-show msg nil nil nil flycheck-pos-tip-timeout nil nil nil line-height))
      (funcall flycheck-pos-tip-display-errors-tty-function errors))))

;;;###autoload
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
       #'ensime-forward-note)))

    (ensime-flycheck-integration--maybe-display-note-popup)))

;;;###autoload
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
       #'flycheck-previous-error)))

    (ensime-flycheck-integration--maybe-display-note-popup)))

;;;###autoload
(defun ensime-flycheck-integration-init ()
  (with-eval-after-load 'ensime
    (define-key ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (define-key ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error))
  (with-eval-after-load 'evil-core
    (evil-define-key 'normal ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)))

(provide 'ensime-flycheck-integration)

;;; ensime-flycheck-integration.el ends here
