;;; rcirc-show-channels.el --- Tile rcirc channel buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((dash "2.12.1") (cl-lib "1.0") (rcirc "22.1"))

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
(require 'rcirc)

(autoload 'eyebrowse-switch-to-window-config "eyebrowse")

(defgroup rcirc-show-channels nil
  "Command for tiling all RCIRC buffers."
  :group 'rcirc
  :prefix "rcirc-show-channels-")

(defcustom rcirc-show-channels-priority nil
  "Alist mapping buffers to display priorities.

Each element is a cons of (CHANNEL . PRIORITY), where CHANNEL is
a string naming a channel (e.g. \"#emacs@irc.freenode.net\") and
PRIORITY is a positive number used to set the display order of
each channel.

Buffers that are not explicitly mentioned here are sorted
alphabetically and assigned negative orders, such that they are
tiled last."
  :group 'rcirc-show-channels
  :type '(alist :key-type string :value-type integer))

(defcustom rcirc-show-channels-eyebrowse-window-config-number nil
  "The eyebrowse window group to use for RCIRC, or nil to disable."
  :group 'rcirc-show-channels
  :type 'number)

(defun rcirc-show-channels--apply-channel-ordering (bufs)
  (->> bufs
       (--sort (string-lessp (buffer-name it) (buffer-name other)))
       (--map-indexed (cons (- it-index) it))
       (--map (-let [(_ . b) it]
                (-if-let ((_ . ord) (assoc (buffer-name b) rcirc-show-channels-priority))
                    (cons ord b)
                  it)))
       (--sort (> (car it) (car other)))
       (-map 'cdr)))

(defun rcirc-show-channels--server-buffers ()
  (->> rcirc-server-alist
       (-map 'car)
       (--keep (get-buffer (format "*%s*" it)))
       (--filter (with-current-buffer it (derived-mode-p 'rcirc-mode it)))))

(defun rcirc-show-channels--channel-buffers ()
  (-difference
   (--filter (with-current-buffer it (derived-mode-p 'rcirc-mode)) (buffer-list))
   (rcirc-show-channels--server-buffers)))

(defun rcirc-show-channels--tile-buffers (bufs)
  (let ((windows
         (if (equal 2 (length bufs))
             (rcirc-show-channels--make-horizontal-split)
           (rcirc-show-channels--make-n-tiles (length bufs)))))
    (cl-assert (equal (length bufs) (length windows)) t)
    (--map-indexed
     (progn
       (select-window (elt windows it-index))
       (switch-to-buffer it))
     bufs)))

(defun rcirc-show-channels--make-n-tiles (n)
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

(defun rcirc-show-channels--make-horizontal-split ()
  (delete-other-windows)
  (let ((w1 (selected-window))
        (w2 (split-window-horizontally)))
    (list w1 w2)))

(defun rcirc-show-channels--scroll-to-bottom (bufs)
  (dolist (b bufs)
    (with-current-buffer b
      (dolist (w (get-buffer-window-list b))
        (select-window w)
        (goto-char (point-max))
        (recenter -1)))))

(defun rcirc-show-channels--maybe-start-rcirc ()
  (unless (rcirc-show-channels--server-buffers)
    (if (fboundp 'spacemacs/rcirc)
        (call-interactively 'spacemacs/rcirc)
      (rcirc nil))))

(defun rcirc-show-channels ()
  "Show all rcirc buffers in a tiled window layout.  Start rcirc if needed.

If rcirc is starting there will be no channel buffers to display.
The server buffer will be displayed instead.  Once channels have
been joined, run the command again to tile the channel buffers.

By default, channel buffers are tiled in alphabetical order.
Customise `rcirc-show-channels-priority' to prioritise certain
channels in the ordering.

If you use eyebrowse, customise
`rcirc-show-channels-eyebrowse-window-config-number' to set which
window config to use for the buffers."
  (interactive)
  (when rcirc-show-channels-eyebrowse-window-config-number
    (eyebrowse-switch-to-window-config rcirc-show-channels-eyebrowse-window-config-number))

  (let ((current-channel
         (--first (and (with-current-buffer it (derived-mode-p 'rcirc-mode))
                       (-contains? (rcirc-show-channels--channel-buffers) it))
                  (buffer-list))))

    (rcirc-show-channels--maybe-start-rcirc)

    (let ((bufs (rcirc-show-channels--apply-channel-ordering (rcirc-show-channels--channel-buffers))))
      (if (null bufs)
          (switch-to-buffer (car (rcirc-show-channels--server-buffers)))
        (rcirc-show-channels--tile-buffers bufs))
      (rcirc-show-channels--scroll-to-bottom bufs))

    (when current-channel
      (select-window (--first (equal (window-buffer it)
                                     current-channel)
                              (window-list))))))

(provide 'rcirc-show-channels)

;;; rcirc-show-channels.el ends here
