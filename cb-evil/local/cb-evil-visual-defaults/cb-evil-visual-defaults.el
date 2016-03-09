;;; cb-evil-visual-defaults.el --- Use visual line navigation by default.  -*- lexical-binding: t; -*-

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

(require 'evil)

(evil-define-motion cb-evil-visual-defaults-last-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (evil-end-of-visual-line)
  (skip-chars-backward " \t\r"))

(defun cb-evil-visual-defaults-append-visual-line (count &optional vcount)
  "Switch to Insert state at the end of the current visual line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (end-of-visual-line count)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-visual-line
                   vcount)))
  (evil-insert-state 1))

(defun cb-evil-visual-defaults-insert-visual-line (count &optional vcount)
  "Switch to insert state at beginning of current visual line.
Point is placed at the first non-blank character on the current
line.  The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (push (point) buffer-undo-list)
  (beginning-of-visual-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'beginning-of-visual-line
                   vcount)))
  (evil-insert-state 1))

(defun cb-evil-visual-defaults-init ()
  (evil-global-set-key 'normal (kbd "j") #'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "k") #'evil-previous-visual-line)
  (evil-global-set-key 'normal (kbd "gj") #'evil-next-line)
  (evil-global-set-key 'normal (kbd "gk") #'evil-previous-line)

  (evil-global-set-key 'normal (kbd "^") #'evil-first-non-blank-of-visual-line)
  (evil-global-set-key 'normal (kbd "$") #'cb-evil-visual-defaults-last-non-blank-of-visual-line)
  (evil-global-set-key 'normal (kbd "g^") #'evil-first-non-blank)
  (evil-global-set-key 'normal (kbd "g$") #'evil-last-non-blank)

  (evil-global-set-key 'normal (kbd "A") #'cb-evil-visual-defaults-append-visual-line)
  (evil-global-set-key 'normal (kbd "I") #'cb-evil-visual-defaults-insert-visual-line))

(provide 'cb-evil-visual-defaults)

;;; cb-evil-visual-defaults.el ends here
