;;; cb-scala-ligatures.el --- Use Hasklig to provide scala ligatures. -*- lexical-binding: t; -*-

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

(defconst cb-scala-ligatures--alist
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


;; Taken from `haskell-font-lock'.
(defun cb-scala-ligatures--compose-symbol (alist)
  "Compose a sequence of ASCII chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun cb-scala-ligatures-init ()
  (when (equal "Hasklig" (font-get (face-attribute 'default :font) :name))
    (font-lock-add-keywords 'scala-mode
                    `((,(regexp-opt (mapcar #'car cb-scala-ligatures--alist) t)
                       (0 (cb-scala-ligatures--compose-symbol ',cb-scala-ligatures--alist) keep))))))

(provide 'cb-scala-ligatures)

;;; cb-scala-ligatures.el ends here
