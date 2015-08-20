;;; extensions.el --- cb-core Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-core-pre-extensions
  '(
    iedit
    smart-ops
    hl-line
    case
    )
  "List of all extensions to load before the packages.")

(defconst cb-core-post-extensions
  '(
    ido
    recentf
    eldoc
    locate-key-binding
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

(defconst cb-core/ignored-files-regexps
  '("\\.elc$"
    "\\.pyc$"
    "TAGS"
    "\\.gz$"
    "flycheck_"
    "\\.DS_Store"
    "\\.swp"
    "#$"
    "^/?sudo"
    "\\.bbdb"
    "\\.newsrc"
    "/gnus$"
    "/gnus.eld$"
    "\\.ido\\.last"
    "\\.org-clock-save\\.el$"))

(defconst cb-core/ignored-dirs
  '(".cabal-sandbox"
    ".idea"
    "dist"
    "target"
    "obj"
    "build"
    "log"
    "logs"
    "tmp"
    "temp"

    ".cache"
    "var/folders"
    "Mail"

    ;; VC
    ".git"
    ".hg"
    ".fslckout"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"

    ;; Emacs
    ".cask"
    "elpa"
    "snippets"
    ".emacs.d/url"
    "Emacs.app"

    ;; Scala
    "project/target"
    "project/project"
    ".ensime_cache"))

(defun cb-core/regexp-quoted-ignored-dirs ()
  (--map (format "/%s/" (regexp-quote it)) cb-core/ignored-dirs))

(defun cb-core/init-recentf ()
  (use-package recentf
    :config
    (progn
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
      (setq recentf-max-saved-items 500)
      (setq recentf-max-menu-items 10)
      (setq recentf-keep '(file-remote-p file-readable-p))

      (setq recentf-exclude
            (-distinct (-concat recentf-exclude
                                (cb-core/regexp-quoted-ignored-dirs)
                                cb-core/ignored-files-regexps)))

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

(defun cb-core/init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (setq eldoc-idle-delay 0.1)))

(defun cb-core/init-smart-ops ()
  (use-package smart-ops
    :diminish smart-ops-mode
    :config
    (progn
      (smart-ops-global-mode)
      (evil-define-key 'insert smart-ops-mode-map (kbd "<backspace>") nil))))

(defun cb-core/init-case ()
  (use-package case))

(defun cb-core/init-locate-key-binding ()
  (use-package locate-key-binding
    :commands (locate-key-binding)))
