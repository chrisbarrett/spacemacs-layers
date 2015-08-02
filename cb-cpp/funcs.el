;;; funcs.el --- cb-cpp Funcs File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 's nil t)
  (require 'dash nil t))

(defun cb-cpp/> ()
  "Insert a '>' and perform context-sensitive formatting."
  (interactive "*")
  (let ((empty-template-rx (rx (+ (not space)) (* space) "<" (* space) eos))
        (pointer-access-rx (rx (+ (not space)) (* space) "-" (* space) eos))
        (line-to-point (buffer-substring (line-beginning-position) (point))))
    (cond
     ((s-matches? empty-template-rx line-to-point)
      (super-smart-ops-delete-last-op)
      (super-smart-ops-insert "<" nil nil)
      (save-excursion
        (super-smart-ops-insert ">" nil nil)))

     ((s-matches? pointer-access-rx line-to-point)
      (super-smart-ops-delete-last-op)
      (insert "->"))

     (t
      (super-smart-ops-insert ">" t t)))))

(defun cb-cpp/: ()
  "Insert a '&' and perform context-sensitive formatting."
  (interactive "*")
  (let ((existing-colon-rx (rx ":" (* space) eos))
        (line-to-point (buffer-substring (line-beginning-position) (point))))
    (cond
     ((s-matches? existing-colon-rx line-to-point)
      (super-smart-ops-delete-last-op)
      (insert "::"))
     (t
      (super-smart-ops-insert ":" nil t)))))

(defun cb-cpp/& ()
  "Insert a '&' and perform context-sensitive formatting."
  (interactive "*")
  (cb-cpp/insert-ptr-like-op "&"))

(defun cb-cpp/* ()
  "Insert a '*' and perform context-sensitive formatting."
  (interactive "*")
  (cb-cpp/insert-ptr-like-op "*"))

(defun cb-cpp/insert-ptr-like-op (op)
  (cond
   ((s-matches? (rx-to-string `(and (not space) (* space) ,op (* space) eos))
                (buffer-substring (line-beginning-position) (point)))
    (super-smart-ops-delete-last-op)
    (super-smart-ops-insert op t t)
    (super-smart-ops-insert op t t))
   (t
    (super-smart-ops-insert op nil t))))

(defun cb-cpp/M-RET ()
  (interactive "*")
  (goto-char (line-end-position))
  (delete-horizontal-space)
  (unless (equal ?\; (char-before))
    (insert ";"))
  (newline-and-indent)
  (evil-insert-state))

(defun cb-cpp/C-RET ()
  (interactive "*")
  (goto-char (line-end-position))
  (delete-horizontal-space)
  (unless (equal ?\; (char-before))
    (insert ";"))
  (evil-insert-state))

(defun cb-cpp/resolve (qualified-name)
  "Return QUALIFIED-NAME for use in snippets.

If that identifier's namespace has been imported, use the name
without qualification."
  (-let [(ns unqualified) (s-split "::" qualified-name)]
    (if (s-matches? (rx-to-string `(or (and bol (* space)
                                            "using" (+ space) "namespace" (+ space) ,ns (* space) ";")
                                       (and "using" (+ space) ,qualified-name (* space) ";")))
                    (buffer-string))
        unqualified
      qualified-name)))
