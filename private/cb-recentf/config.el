(require 'f)
(require 'noflet)

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
  (noflet ((message (&rest args))) ad-do-it))

(recentf-mode +1)
