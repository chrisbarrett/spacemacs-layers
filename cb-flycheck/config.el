;;; config.el --- Configuration for flycheck
;;; Commentary:
;;; Code:

(require 's nil t)
(require 'dash nil t)
(require 'flycheck nil t)

;;; Hide noisy Haskell file paths from error output

(defconst cb-flycheck--src-file-rx
  (rx bol (+ space) (? "(bound ") "at /" (* nonl) eol))

(defun cb-flycheck--strip-filepath (str)
  (->> (s-split "\n" str)
       (--remove (s-matches? cb-flycheck--src-file-rx it))
       (s-join "\n")))

(defun cb-flycheck-strip-files-in-messages (errors)
  (dolist (err errors)
    (let ((msg (cb-flycheck--strip-filepath (flycheck-error-message err))))
      (setf (flycheck-error-message err) msg)))
  errors)

(with-eval-after-load 'flycheck
  (put 'haskell-stack-ghc
       'flycheck-error-filter
       (lambda (errors)
         (-> errors
             flycheck-dedent-error-messages
             cb-flycheck-strip-files-in-messages
             flycheck-sanitize-errors)))

  (put 'haskell-ghc
       'flycheck-error-filter
       (lambda (errors)
         (-> errors
             flycheck-dedent-error-messages
             cb-flycheck-strip-files-in-messages
             flycheck-sanitize-errors))))

(with-eval-after-load 'flycheck-liquid
  (put 'haskell-liquid
       'flycheck-error-filter
       (lambda (errors)
         (-> errors
             flycheck-dedent-error-messages
             cb-flycheck-strip-files-in-messages
             flycheck-sanitize-errors))))

;;; config.el ends here
