;;; sp-in-minibuffer.el --- Enable smartparens for certain minibuffer commands. -*- lexical-binding: t; -*-

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

;; Enable smartparens for certain minibuffer commands.

;;; Code:

(require 'dash)

(autoload 'smartparens-mode "smartparens")

(defgroup sp-in-minibuffer nil
  "Enable smartparens for certain minibuffer commands."
  :group 'editing
  :prefix "sp-in-minibuffer-")

(defcustom sp-in-minibuffer-enabled-commands
  '(eval-expression
    calc-algebraic-entry
    quick-calc
    debugger-eval-expression)
  "Commands that take input in the minibuffer for which smartparens should be used."
  :group 'sp-in-minibuffer
  :type '(repeat function))

(defun sp-in-minibuffer--maybe-enable-smartparens ()
  (if (-contains? sp-in-minibuffer-enabled-commands this-command)
      (smartparens-mode +1)
    (smartparens-mode -1)))

;;;###autoload
(defun sp-in-minibuffer-init ()
  (add-hook 'minibuffer-setup-hook #'sp-in-minibuffer--maybe-enable-smartparens t)
  (add-hook 'minibuffer-inactive-mode-hook #'sp-in-minibuffer--maybe-enable-smartparens t))

(provide 'sp-in-minibuffer)

;;; sp-in-minibuffer.el ends here
