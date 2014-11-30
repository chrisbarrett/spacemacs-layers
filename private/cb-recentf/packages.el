(defvar cb-recentf-packages
  '(
    ;; package cb-recentfs go here
    recentf
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-recentf-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-recentf/init-<package-cb-recentf>
;;
;; (defun cb-recentf/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-recentf/init-recentf ()
  (use-package recentf
    :commands recentf-mode
    :config
    (progn
      (custom-set-variables
       '(recentf-save-file (f-join spacemacs-cache-directory "recentf"))
       '(recentf-max-saved-items 50)
       '(recentf-max-menu-items 10)
       '(recentf-keep '(file-remote-p file-readable-p))
       '(recentf-exclude
         '("\\.elc$"
           "TAGS"
           "\\.gz$"
           "#$"
           "/elpa/"
           "/tmp/"
           "/temp/"
           "/snippets/"
           ".emacs.d/url/"
           "/\\.git/"
           "/Emacs.app/"
           "/var/folders/"
           "^/?sudo"
           "\\.bbdb"
           "\\.newsrc"
           "/gnus$"
           "/gnus.eld$"
           "\\.ido\\.last"
           "\\.org-clock-save\\.el$")))

      (defadvice recentf-cleanup (around hide-messages activate)
        "Do not message when cleaning up recentf list."
        (noflet ((message (&rest args))) ad-do-it)))))
