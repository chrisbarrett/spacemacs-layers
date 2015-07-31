;;; funcs.el --- cb-cpp Funcs File for Spacemacs
;;; Commentary:
;;; Code:

(defun cb-cpp/> ()
  "Insert a '>' and perform context-sensitive formatting."
  (interactive "*")
  (super-smart-ops-insert ">")
  (let ((empty-template-rx
         (rx (+ (not space)) (* space) "<" (* space) ">" (* space) eos))
        (line-to-point (buffer-substring (line-beginning-position) (point))
                       ))
    (when (s-matches? empty-template-rx line-to-point)
      (search-backward "<")
      (delete-horizontal-space)
      (search-forward "<")
      (delete-horizontal-space))))
