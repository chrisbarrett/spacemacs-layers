;;; packages.el --- cb-ibuffer Layer packages File for Spacemacs
;;; Commentary:
;;;
;;; Stolen shamelessly from TheBB's configuration.
;;;     https://github.com/TheBB/spacemacs-layers/blob/master/bb/bb-ibuffer/packages.el
;;;
;;; Code:

(eval-when-compile
  (require 'dash nil t))

(defconst cb-ibuffer-packages
  '(ibuffer))

(defun cb-ibuffer/pre-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (require 'projectile)
    (setq ibuffer-never-show-predicates
          `("^\\*inferior-ensime"
            "^\\*ensime-update"
            "^\\*helm"))

    (setq ibuffer-saved-filter-groups
          (let ((files-by-project
                 (--map `(,(f-filename it) (filename . ,(f-expand it))) projectile-known-projects)))
            `(("default"
               ,@files-by-project
               ("dired" (mode . dired-mode))
               ("emacs" (or (name . "\\*Messages\\*")
                            (name . "\\*Compile-Log\\*")
                            (name . "\\*scratch\\*")
                            (name . "\\*Backtrace\\*")
                            (name . "\\*spacemacs\\*")
                            (name . "\\*emacs\\*")))
               ("help" (name . "\\*Help\\*"))))))))

(defun cb-ibuffer/post-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (setq ibuffer-show-empty-filter-groups nil)

    (defun cb-ibuffer/switch-ibuffer-group ()
      (ibuffer-switch-to-saved-filter-groups "default"))

    (add-hook 'ibuffer-mode-hook 'cb-ibuffer/switch-ibuffer-group)
    (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    ;; Modify the default ibuffer-formats
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)))))
