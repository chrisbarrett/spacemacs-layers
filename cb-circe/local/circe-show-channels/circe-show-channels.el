;;; circe-show-channels.el --- Tile circe channel buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((dash "2.12.1") (cl-lib "1.0") (circe "22.1"))

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

(require 'cl-lib)
(require 'dash)
(require 'circe)

(defgroup circe-show-channels nil
  "Command for tiling all Circe buffers."
  :group 'circe
  :prefix "circe-show-channels-")

(defcustom circe-show-channels-priority nil
  "Alist mapping buffers to display priorities.

Each element is a cons of (CHANNEL . PRIORITY), where CHANNEL is
a string naming a channel (e.g. \"#emacs@irc.freenode.net\") and
PRIORITY is a positive number used to set the display order of
each channel.

Buffers that are not explicitly mentioned here are sorted
alphabetically and assigned negative orders, such that they are
tiled last."
  :group 'circe-show-channels
  :type '(alist :key-type string :value-type integer))

(defun circe-show-channels--apply-buffer-ordering (bufs)
  (->> bufs
       (--sort (string-lessp (buffer-name it) (buffer-name other)))
       (--map-indexed (cons (- it-index) it))
       (--map (-let [(_ . b) it]
                (-if-let ((_ . ord) (assoc (buffer-name b) circe-show-channels-priority))
                    (cons ord b)
                  it)))
       (--sort (> (car it) (car other)))
       (-map 'cdr)))

(defun circe-show-channels--chat-buffers ()
  (--filter (with-current-buffer it
              (derived-mode-p 'circe-chat-mode))
            (buffer-list)))

(defun circe-show-channels--tile-buffers (bufs)
  (let ((windows
         (if (equal 2 (length bufs))
             (circe-show-channels--make-horizontal-split)
           (circe-show-channels--make-n-tiles (length bufs)))))
    (cl-assert (equal (length bufs) (length windows)) t)
    (--map-indexed
     (progn
       (select-window (elt windows it-index))
       (switch-to-buffer it))
     bufs)))

(defun circe-show-channels--make-n-tiles (n)
  "Create a tiled window with N entries.

Create horizontal splits, then split each of those at most once
until there are exactly `n' windows in this frame.

Return the created windows."
  (delete-other-windows)
  (let* ((evens
          (-iterate (lambda (_)
                      (split-window-horizontally))
                    (selected-window)
                    (ceiling (/ n 2.0))))
         (odds
          (->> (-drop (cl-rem n 2) evens)
               (--map (progn
                        (select-window it)
                        (split-window-vertically)))))
         (results
          (->> (-zip-fill nil evens odds)
               (--reduce-from (cons (cdr it) (cons (car it) acc)) nil)
               (nreverse)
               (-non-nil))))
    (cl-assert (-all? #'windowp results) t)
    results))

(defun circe-show-channels--make-horizontal-split ()
  (delete-other-windows)
  (let ((w1 (selected-window))
        (w2 (split-window-horizontally)))
    (list w1 w2)))

(defun circe-show-channels--scroll-to-bottom (bufs)
  (dolist (b bufs)
    (with-current-buffer b
      (dolist (w (get-buffer-window-list b))
        (select-window w)
        (goto-char (point-max))
        (recenter -1)))))

(defun circe-show-channels--maybe-start-circe ()
  (unless (circe-server-buffers)
    (-each (-map #'car circe-network-options) #'circe)))

(defun circe-show-channels ()
  "Show all circe buffers in a tiled window layout.  Start circe if needed.

If circe is starting there will be no channel buffers to display.
The server buffer will be displayed instead.  Once channels have
been joined, run the command again to tile the channel buffers.

By default, channel buffers are tiled in alphabetical order.
Customise `circe-show-channels-priority' to prioritise certain
channels in the ordering."
  (interactive)

  (let ((current-chat
         (--first (with-current-buffer it
                    (derived-mode-p 'circe-chat-mode))
                  (buffer-list))))

    (circe-show-channels--maybe-start-circe)

    (let ((bufs (circe-show-channels--apply-buffer-ordering (circe-show-channels--chat-buffers))))
      (if (null bufs)
          (switch-to-buffer (car (circe-server-buffers)))
        (circe-show-channels--tile-buffers bufs))
      (circe-show-channels--scroll-to-bottom bufs))

    (when current-chat
      (select-window (--first (equal (window-buffer it)
                                     current-chat)
                              (window-list))))))

(provide 'circe-show-channels)

;;; circe-show-channels.el ends here
