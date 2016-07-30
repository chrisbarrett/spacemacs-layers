;;; cb-hasklig.el --- Hasklig support for Haskell mode.  -*- lexical-binding: t; -*-

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

;; Hasklig is a derivative of Source Code Pro, with additional ligatures for
;; common symbols in Haskell.
;;
;;   https://github.com/i-tu/Hasklig
;;
;; Emacs integration based on:
;;
;;   http://lpaste.net/153250

;;; Code:

(require 'haskell-font-lock)

(defconst cb-hasklig-ligature-alist
  (list (cons "&&" (decode-char 'ucs #XE100))
        (cons "***" (decode-char 'ucs #XE101))
        (cons "*>" (decode-char 'ucs #XE102))
        (cons "\\\\" (decode-char 'ucs #XE103))
        (cons "||" (decode-char 'ucs #XE104))
        (cons "|>" (decode-char 'ucs #XE105))
        (cons "::" (decode-char 'ucs #XE106))
        (cons "==" (decode-char 'ucs #XE107))
        (cons "===" (decode-char 'ucs #XE108))
        (cons "==>" (decode-char 'ucs #XE109))
        (cons "=>" (decode-char 'ucs #XE10A))
        (cons "=<<" (decode-char 'ucs #XE10B))
        (cons "!!" (decode-char 'ucs #XE10C))
        (cons ">>" (decode-char 'ucs #XE10D))
        (cons ">>=" (decode-char 'ucs #XE10E))
        (cons ">>>" (decode-char 'ucs #XE10F))
        (cons ">>-" (decode-char 'ucs #XE110))
        (cons ">-" (decode-char 'ucs #XE111))
        (cons "->" (decode-char 'ucs #XE112))
        (cons "-<" (decode-char 'ucs #XE113))
        (cons "-<<" (decode-char 'ucs #XE114))
        (cons "<*" (decode-char 'ucs #XE115))
        (cons "<*>" (decode-char 'ucs #XE116))
        (cons "<|" (decode-char 'ucs #XE117))
        (cons "<|>" (decode-char 'ucs #XE118))
        (cons "<$>" (decode-char 'ucs #XE119))
        (cons "<>" (decode-char 'ucs #XE11A))
        (cons "<-" (decode-char 'ucs #XE11B))
        (cons "<<" (decode-char 'ucs #XE11C))
        (cons "<<<" (decode-char 'ucs #XE11D))
        (cons "<+>" (decode-char 'ucs #XE11E))
        (cons ".." (decode-char 'ucs #XE11F))
        (cons "..." (decode-char 'ucs #XE120))
        (cons "++" (decode-char 'ucs #XE121))
        (cons "+++" (decode-char 'ucs #XE122))
        (cons "/=" (decode-char 'ucs #XE123))))

;;;###autoload
(defun cb-hasklig-init ()
  (when (equal "Hasklig" (font-get (face-attribute 'default :font) :name))
    (setq haskell-font-lock-symbols t)
    (setq haskell-font-lock-symbols-alist cb-hasklig-ligature-alist)))

(provide 'cb-hasklig)

;;; cb-hasklig.el ends here
