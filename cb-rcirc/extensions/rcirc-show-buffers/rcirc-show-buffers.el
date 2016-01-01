;;; rcirc-show-buffers.el --- Tile rcirc buffers  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 'rcirc)
(require 'cl-lib)
(autoload 'eyebrowse-switch-to-window-config "eyebrowse")

(defgroup rcirc-show-buffers nil
  "Command for tiling all RCIRC buffers."
  :group 'rcirc
  :prefix "rcirc-show-buffers-")

(defcustom rcirc-show-buffers-priority nil
  "Alist mapping buffers to display priorities.

Each element is a cons of (CHANNEL . PRIORITY), where CHANNEL is
a string naming a channel (e.g. \"#emacs\") and PRIORITY is a
positive number used to set the display order of each channel.

Buffers that are not explicitly mentioned here are sorted
alphabetically and assigned negative orders, such that they are
tiled last."
  :group 'rcirc-show-buffers
  :type '(alist :key-type string :value-type integer))

(defcustom rcirc-show-buffers-eyebrowse-window-config-number nil
  "The eyebrowse window group to use for RCIRC, or nil to disable."
  :group 'rcirc-show-buffers
  :type 'number)

(defun rcirc-show-buffers--apply-channel-ordering (bufs)
  (->> bufs
       (--sort (string-lessp (buffer-name it) (buffer-name other)))
       (--map-indexed (cons (- it-index) it))
       (--map (-let [(_ . b) it]
                (-if-let ((_ . ord) (assoc (buffer-name b) rcirc-show-buffers-priority))
                    (cons ord b)
                  it)))
       (--sort (> (car it) (car other)))
       (-map 'cdr)))

(defun rcirc-show-buffers--server-buffers ()
  (->> rcirc-server-alist
       (-map 'car)
       (--keep (get-buffer (format "*%s*" it)))
       (--filter (with-current-buffer it (derived-mode-p 'rcirc-mode it)))))

(defun rcirc-show-buffers--channel-buffers ()
  (->> (-difference
        (--filter (with-current-buffer it (derived-mode-p 'rcirc-mode)) (buffer-list))
        (rcirc-show-buffers--server-buffers))))

(defun rcirc-show-buffers--tile-buffers (bufs)
  (cl-labels ((go (bufs-and-indices)
                  (-let [((idx . b) . bs) bufs-and-indices]
                    (switch-to-buffer b)
                    (when bs
                      (let ((w (if (cl-evenp idx) (split-window-horizontally) (split-window-vertically))))
                        (select-window w)
                        (go bs))))))
    (delete-other-windows)
    (go (--map-indexed (cons it-index it) bufs))))

(defun rcirc-show-buffers--scroll-to-bottom (bufs)
  (dolist (b bufs)
    (with-current-buffer b
      (dolist (w (get-buffer-window-list b))
        (select-window w)
        (goto-char (point-max))
        (recenter -1)))))

(defun rcirc-show-buffers--maybe-start-rcirc ()
  (unless (rcirc-show-buffers--server-buffers)
    (if (fboundp 'spacemacs/rcirc)
        (call-interactively 'spacemacs/rcirc)
      (rcirc nil))))

(defun rcirc-show-buffers ()
  "Switch to eyebrowse workspace 1 and show all rcirc buffers."
  (interactive)
  (when rcirc-show-buffers-eyebrowse-window-config-number
    (eyebrowse-switch-to-window-config rcirc-show-buffers-eyebrowse-window-config-number))
  (rcirc-show-buffers--maybe-start-rcirc)
  (let ((bufs (rcirc-show-buffers--apply-channel-ordering (rcirc-show-buffers--channel-buffers))))
    (rcirc-show-buffers--tile-buffers bufs)
    (rcirc-show-buffers--scroll-to-bottom bufs)))

(provide 'rcirc-show-buffers)

;;; rcirc-show-buffers.el ends here
