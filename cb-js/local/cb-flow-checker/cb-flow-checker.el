;;; cb-flow-checker.el --- A flycheck checker for Flow.  -*- lexical-binding: t; -*-

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

;; Almost a direct copy-paste from Lorenzo Bolla's implementation <lbolla@gmail.com>:
;;
;;   https://github.com/lbolla/emacs-flycheck-flow/blob/master/flycheck-flow.el

;;; Code:

(require 'flycheck)

(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)
(customize-set-variable 'flycheck-javascript-flow-args '("status"))

(flycheck-define-checker javascript-flow
  "FLycheck checker for Facebook's Flow type inferencer for Javascript."
  :command ("flow"
            (eval flycheck-javascript-flow-args)
            "--old-output-format"
            "--color=never"
            source-original)
  :error-patterns
  ((error line-start
          (file-name)
          ":" line
          ":" (minimal-match (one-or-more not-newline))
          ": " (message (minimal-match (and (one-or-more anything) "\n"))) line-end))
  :modes (js-mode js2-mode cb-web-js-mode)
  :predicate (lambda ()
               (locate-dominating-file default-directory ".flowconfig")))

(add-to-list 'flycheck-checkers 'javascript-flow t)

(provide 'cb-flow-checker)

;;; cb-flow-checker.el ends here
