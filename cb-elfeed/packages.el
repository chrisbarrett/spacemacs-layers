;;; packages.el --- cb-elfeed Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-elfeed-packages '(elfeed)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-elfeed-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-elfeed/init-elfeed ()
  (use-package elfeed
    :commands elfeed
    :init
    (evil-leader/set-key "af" 'elfeed)
    :config
    (progn
      (evil-set-initial-state 'elfeed-search-mode 'emacs)
      (evil-set-initial-state 'elfeed-show-mode 'emacs)

      (defun cb-elfeed/search-mode-browse-external-browser ()
        (interactive)
        (let ((browse-url-browser-function 'browse-url-default-browser))
          (call-interactively 'elfeed-search-browse-url)))

      (defun cb-elfeed/show-visit-external-browser ()
        (interactive)
        (let ((browse-url-browser-function 'browse-url-default-browser))
          (call-interactively 'elfeed-show-visit)))

      (evilify elfeed-search-mode elfeed-search-mode-map
               (kbd "q") 'quit-window
               (kbd "&") 'cb-elfeed/search-mode-browse-external-browser)

      (evilify elfeed-show-mode elfeed-show-mode-map
               (kbd "q") 'elfeed-kill-buffer
               (kbd "&") 'cb-elfeed/show-visit-external-browser)

      (defconst cb-elfeed/update-timer
        (run-with-timer 1 (* 60 60) 'elfeed-update)))))
