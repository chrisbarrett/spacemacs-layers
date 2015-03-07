;;; Hide tmp file paths from error output

(defconst cb-flycheck--src-file-rx
  (rx bol (+ space) (? "(bound ") "at /var/" (* nonl) eol))

(defun cb-flycheck--strip-tmp-filepath (str)
  (->> (s-split "\n" str)
       (--remove (s-matches? cb-flycheck--src-file-rx it))
       (s-join "\n")))

(defun cb-flycheck-strip-tmpfiles-in-messages (errors)
  (dolist (err errors)
    (let ((message (cb-flycheck--strip-tmp-filepath (flycheck-error-message err))))
      (setf (flycheck-error-message err) message)))
  errors)

(after 'flycheck
  (put 'haskell-ghc
       'flycheck-error-filter
       (lambda (errors)
         (flycheck-sanitize-errors (cb-flycheck-strip-tmpfiles-in-messages (flycheck-dedent-error-messages errors))))))
