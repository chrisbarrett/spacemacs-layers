;;; cb-stack-hoogle.el --- Adapt haskell-hoogle to use stack.  -*- lexical-binding: t; -*-

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

(require 'haskell-hoogle)

;;;###autoload
(defun cb-stack-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then hoogle is asked to show
extra info for the items matching QUERY.."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def)
           current-prefix-arg)))
  (let ((command (format "stack hoogle -- --colour %s %s"
                         (if info " -i " "")
                         (shell-quote-argument query))))
    (with-help-window "*stack hoogle*"
      (with-current-buffer standard-output
        (insert (shell-command-to-string command))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(provide 'cb-stack-hoogle)

;;; cb-stack-hoogle.el ends here
