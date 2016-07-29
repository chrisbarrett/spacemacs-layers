;;; funcs.el --- Helper functions for cb-core layer -*- lexical-binding: t; -*-
;;; Documentation:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)

(autoload 'helm "helm-command")

;;; Exiting Emacs

(defun cb-core-regexp-quoted-ignored-dirs ()
  (--map (format "/%s/" (regexp-quote it)) cb-vars-ignored-dirs))

;;; HACK: override Spacemacs function to prevent M-RET from being bound.

(defun spacemacs/activate-major-mode-leader ()
  "Bind major mode key map to `dotspacemacs-major-mode-leader-key'."
  (setq mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
  (when mode-map
    (setq major-mode-map (lookup-key mode-map (kbd "m")))
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-local-map" s))
                     ,(kbd dotspacemacs-major-mode-leader-key)
                     major-mode-map)))
          '(normal motion))))

;;; funcs.el ends here
