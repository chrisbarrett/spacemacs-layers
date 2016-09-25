;;; cb-helm-emoticons.el --- Emoticon picker using helm. -*- lexical-binding: t; -*-

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

(require 'helm)

(defun cb-helm-emoticons-entry (name emoticon)
  (cons (format "%s\n  %s" name emoticon) emoticon))

(defconst cb-helm-emoticons-alist
  (mapcar (lambda (it) (cb-helm-emoticons-entry (car it) (cdr it)))
          '(("shrug" . "¯\\_(ツ)_/¯")
            ("table flip 1" . "(╯°□°）╯︵ ┻━┻")
            ("table flip 2" . "(ノಠ益ಠ)ノ彡┻━┻")
            ("put that table back" . "┬──┬ ノ( ゜-゜ノ)")
            ("give" . "༼ つ ◕_◕ ༽つ")
            ("cry" . "༼ ༎ຶ ෴ ༎ຶ༽"))))

(defconst cb-helm-emoticons--source
  (helm-build-sync-source "Emoticons"
    :multiline t
    :candidates cb-helm-emoticons-alist
    :action (lambda (emoticon)
              (kill-new emoticon)
              (message "Copied to kill-ring: %s" emoticon))))

;;;###autoload
(defun cb-helm-emoticons ()
  "Show an emoticon picker."
  (interactive)
  (helm
   :buffer "*helm emoticons*"
   :sources cb-helm-emoticons--source
   :prompt "Emoji (RET to copy): "))

(provide 'cb-helm-emoticons-alist)

;;; cb-helm-emoticons.el ends here
