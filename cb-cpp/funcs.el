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
