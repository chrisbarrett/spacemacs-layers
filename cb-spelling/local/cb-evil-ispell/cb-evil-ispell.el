;;; cb-evil-ispell.el --- Evil extensions for ispell mode  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'flyspell)
(require 'ispell)
(require 's)
(require 'dash)

(autoload 'evil-global-set-key "evil-core")

(defun cb-evil-ispell--add-to-dict (word)
  "Add WORD to the user's dictionary."
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t))
  (ispell-pdict-save ispell-silently-savep))

(defun cb-evil-ispell-mark-word-as-good (word)
  "Add WORD at point to the Ispell dictionary."
  (interactive (list (thing-at-point 'word)))
  (cb-evil-ispell--add-to-dict word)
  (message "%s added to dictionary" (s-upcase word)))

(defun cb-evil-ispell-correct-word (arg)
  "Corect the word at point with Ispell.
With a number ARG, select the nth replacement."
  (interactive "*P")
  (if (numberp arg)
      (dotimes (_ (1+ arg))
        (flyspell-auto-correct-word))
    (ispell-word)))

(defun cb-evil-ispell-mark-word-as-locally-good (word)
  "Add WORD at point to the list of locally-defined words."
  (interactive (list (thing-at-point 'word)))
  (when word
    (ispell-add-per-file-word-list word)
    (message "%s added to local word list" (s-upcase word))))

(defun cb-evil-ispell--error-backward-search-start-pos (pos)
  "Wrap the search to the end of the buffer if there are no
errors before POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-min))
        (message "Restarting from end of buffer")
        (point-max))
       (t
        (save-excursion
          (forward-word -1)
          (point))))
    (point)))

(defun cb-evil-ispell--prev-spelling-error-pos ()
  (let ((pos (cb-evil-ispell--error-backward-search-start-pos (point))))
    (while (and (> pos (point-min))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-decf pos))
    pos))

(defun cb-evil-ispell-previous-spelling-error ()
  "Go to the previous flyspell error."
  (interactive)
  (let ((pos (cb-evil-ispell--prev-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-min))
      (message "No more spelling errors"))))

(defun cb-evil-ispell--error-forward-search-start-pos (pos)
  "Wrap the search to the beginning of the buffer if there are no
errors forward of POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-max))
        (message "Restarting from beginning of buffer")
        (point-min))
       (t
        (save-excursion
          (forward-word 1)
          (point))))
    (point)))

(defun cb-evil-ispell--next-spelling-error-pos ()
  (let ((pos (cb-evil-ispell--error-forward-search-start-pos (point))))
    (while (and (< pos (point-max))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-incf pos))
    pos))

(defun cb-evil-ispell-next-spelling-error ()
  "Go to the next flyspell error."
  (interactive)
  (let ((pos (cb-evil-ispell--next-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-max))
      (message "No more spelling errors"))))

(defun cb-evil-ispell-init ()
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "[s")  #'cb-evil-ispell-previous-spelling-error)
    (evil-global-set-key 'normal (kbd "]s")  #'cb-evil-ispell-next-spelling-error)
    (evil-global-set-key 'normal (kbd "z g") #'cb-evil-ispell-mark-word-as-good)
    (evil-global-set-key 'normal (kbd "z G") #'cb-evil-ispell-mark-word-as-locally-good)
    (evil-global-set-key 'normal (kbd "z =") #'cb-evil-ispell-correct-word)
    (evil-global-set-key 'normal (kbd "z u") #'flyspell-auto-correct-word)))

(provide 'cb-evil-ispell)

;;; cb-evil-ispell.el ends here
