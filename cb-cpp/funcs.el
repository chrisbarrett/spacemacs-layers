;;; funcs.el --- cb-cpp Funcs File for Spacemacs
;;; Commentary:
;;; Code:

(defun cb-cpp/> ()
  "Insert a '>' and perform context-sensitive formatting."
  (interactive "*")
  (super-smart-ops-insert ">" nil nil)
  (let ((empty-template-rx
         (rx (+ (not space)) (* space) "<" (* space) ">" (* space) eos))
        (line-to-point (buffer-substring (line-beginning-position) (point))
                       ))
    (when (s-matches? empty-template-rx line-to-point)
      (super-smart-ops-delete-last-op)
      (super-smart-ops-delete-last-op)
      (super-smart-ops-insert "<" nil nil)
      (save-excursion
        (super-smart-ops-insert ">" nil nil)))))

(defun cb-cpp/& ()
  "Insert a '>' and perform context-sensitive formatting."
  (interactive "*")
  (cond
   ((s-matches? (rx (not space) (* space) "&" (* space) eos)
                (buffer-substring (line-beginning-position) (point)))
    (save-excursion
      (super-smart-ops-delete-last-op)
      (super-smart-ops-insert "&" t t)
      (super-smart-ops-insert "&" t t)))
   (t
    (super-smart-ops-insert "&" nil t))))
