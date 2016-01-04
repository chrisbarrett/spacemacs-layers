;;; lazy-sml-mode.el --- Major mode for SML with laziness extensions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

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

(require 'sml-mode)

;;;###autoload
(define-derived-mode lazy-sml-mode sml-mode "SML[lazy]"
  "Extend `sml-mode' with support for laziness language extensions."
  nil

  ;; Enable support for laziness language extensions. Requires SMLNJ.
  (setq-local sml-default-arg "-Cparser.lazy-keyword=true")
  (setq-local flycheck-sml-nj-args '("-Cparser.lazy-keyword=true")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lml\\'" . lazy-sml-mode))

(font-lock-add-keywords
 'lazy-sml-mode
 `((,(rx bol (* space) "fun" (+ space) (group "lazy") (+ space) (group (+ word)) eow)
    (1 font-lock-type-def-face)
    (2 font-lock-function-name-face))))

(provide 'lazy-sml-mode)

;;; lazy-sml-mode.el ends here
