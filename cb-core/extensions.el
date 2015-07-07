;;; extensions.el --- cb-core Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-core-pre-extensions
  '(
    iedit
    super-smart-ops
    hl-line
    )
  "List of all extensions to load before the packages.")

(defconst cb-core-post-extensions
  '(
    ido
    recentf
    )
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t)
  (require 's)
  (require 'f)
  (require 'dash))

;; Add extension subdirs to load-path
(--each (f-directories (f-join user-layers-directory "cb-core/extensions/"))
  (push it load-path))

(defun cb-core/init-super-smart-ops ()
  (use-package super-smart-ops))

(defun cb-core/init-ido ()
  (use-package ido
    :config
    (progn
      (setq ido-use-filename-at-point 'guess)
      (add-to-list 'ido-ignore-buffers "\\*helm.*")
      (add-to-list 'ido-ignore-buffers "\\*Minibuf.*")
      (add-to-list 'ido-ignore-files (rx bos "Icon" control))
      (add-to-list 'ido-ignore-files "flycheck_")
      (add-to-list 'ido-ignore-files "\\.swp")
      (add-to-list 'ido-ignore-files "\\.DS_Store"))))

(defun cb-core/init-recentf ()
  (use-package recentf
    :config
    (progn
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
      (setq recentf-max-saved-items 50)
      (setq recentf-max-menu-items 10)
      (setq recentf-keep '(file-remote-p file-readable-p))

      (setq recentf-exclude
            (append recentf-exclude
                    '("\\.elc$"
                      "\\.pyc$"
                      "TAGS"
                      "\\.gz$"
                      "#$"
                      "/elpa/"
                      "/log/"
                      "/logs/"
                      "/tmp/"
                      "/temp/"
                      "/target/"
                      "/snippets/"
                      ".emacs.d/url/"
                      "/\\.git/"
                      "/Emacs.app/"
                      "/var/folders/"
                      "^/?sudo"
                      "\\.bbdb"
                      "/\\.cache/"
                      "\\.newsrc"
                      "/gnus$"
                      "/Mail/"
                      "/gnus.eld$"
                      "\\.ido\\.last"
                      "\\.org-clock-save\\.el$")))

      (defadvice recentf-cleanup (around hide-messages activate)
        "Do not message when cleaning up recentf list."
        (noflet ((message (&rest args))) ad-do-it))

      (recentf-cleanup))))

(defun cb-core/init-iedit ()
  (use-package iedit
    :config
    (custom-set-faces
     `(iedit-occurrence ((t (:background ,solarized-hl-orange :foreground "white")))))))

(defun cb-core/init-hl-line ()
  (use-package hl-line
    :config
    (global-hl-line-mode -1)))
