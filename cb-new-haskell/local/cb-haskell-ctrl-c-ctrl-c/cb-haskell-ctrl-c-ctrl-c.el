;;; cb-haskell-ctrl-c-ctrl-c.el --- <enter description here>  -*- lexical-binding: t; -*-

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
(require 'haskell)
(require 'haskell-mode)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-jump-item "evil-commands")
(autoload 'sp-get-enclosing-sexp "smartparens")

(defun cb-haskell-ctrl-c-ctrl-c--at-import? ()
  (save-excursion
    (-when-let ((&plist :beg beg) (sp-get-enclosing-sexp))
      (goto-char beg))
    (when (s-matches? (rx bol (? ">" (* space)) "import" (+ space)) (cb-buffers-current-line))
      (line-beginning-position))))

(defun cb-haskell-ctrl-c-ctrl-c--at-simple-import? ()
  (-when-let (pos (cb-haskell-ctrl-c-ctrl-c--at-import?))
    (goto-char pos)
    (not (s-matches? (rx bol (? ">" (* space)) "import" (+ space) "qualified") (cb-buffers-current-line)))))

(defun cb-haskell-ctrl-c-ctrl-c--simple-to-qualified-import (import-parts-plist)
  (plist-put import-parts-plist :import "import qualified"))

(defun cb-haskell-ctrl-c-ctrl-c--qualified-to-simple-import (import-parts-plist)
  (--> import-parts-plist
       (plist-put it :import "import")
       (plist-put it :as nil)))

(defun cb-haskell-ctrl-c-ctrl-c--render-import-to-string (import-parts-plist)
  (-let [(&plist :birdtracks tracks :import import :module module :as as :members members) import-parts-plist]
    (->> (list tracks import module as members)
         (-remove #'s-blank?)
         (s-join " "))))

(defun cb-haskell-ctrl-c-ctrl-c--parse-import-to-plist (str)
  (-let [(_ birdtracks import module as members)
         (s-match (rx bol
                      (group (? ">"))
                      (* space)
                      (group "import" (? (+ space) "qualified"))
                      (* space)
                      (group (+ (any alnum "." "_")))
                      (* space)
                      (group (? (and "as" (+ space (+ (any alnum "_"))))))
                      (* space)
                      (group (* anything)))
                  str)]
    (list :birdtracks birdtracks
          :import import
          :module module
          :as as
          :members members)))

(defun cb-haskell-ctrl-c-ctrl-c--import-at (beg)
  (-let* ((end (save-excursion
                 (goto-char beg)
                 (cond
                  ((s-contains? "(" (cb-buffers-current-line))
                   (goto-char (line-beginning-position))
                   (search-forward "(")
                   (plist-get (sp-get-enclosing-sexp) :end))
                  (t
                   (line-end-position)))))
          (str (buffer-substring-no-properties beg end)))
    (list :beg beg
          :end end
          :str str
          :parts (cb-haskell-ctrl-c-ctrl-c--parse-import-to-plist str))))

(defun cb-haskell-ctrl-c-ctrl-c--toggle-import-qualification (import)
  (save-excursion
    (-let* (((&plist :parts parts :beg beg :end end) import)
            (updated (if (cb-haskell-ctrl-c-ctrl-c--at-simple-import?)
                         (cb-haskell-ctrl-c-ctrl-c--simple-to-qualified-import parts)
                       (cb-haskell-ctrl-c-ctrl-c--qualified-to-simple-import parts))))
      (delete-region beg end)
      (insert (cb-haskell-ctrl-c-ctrl-c--render-import-to-string updated)))))

;;;###autoload
(defun cb-haskell-ctrl-c-ctrl-c ()
  "Perform a context-sensitive refactoring command."
  (interactive)
  (-if-let (import (-some->> (cb-haskell-ctrl-c-ctrl-c--at-import?)
                             (cb-haskell-ctrl-c-ctrl-c--import-at)))
      (cb-haskell-ctrl-c-ctrl-c--toggle-import-qualification import)
    (user-error "No action to perform here")))

(provide 'cb-haskell-ctrl-c-ctrl-c)

;;; cb-haskell-ctrl-c-ctrl-c.el ends here
