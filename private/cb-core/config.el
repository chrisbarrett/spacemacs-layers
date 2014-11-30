(require 'f)
(require 'dash)
(require 'dash-functional)

;; Menu-bar looks acceptable in OS X. Otherwise it adds clutter.
(when (fboundp 'menu-bar-mode)
  (unless (and (eq system-type 'darwin)
               (not noninteractive))
    (menu-bar-mode -1)))


;;; Use larger font
(spacemacs/set-font "SourceCodePro" 12)

(defvar spacemacs-private-directory (concat user-emacs-directory "private/"))
(defvar spacemacs-autosaves-directory (concat user-emacs-directory "autosaves/")) 

;;; Compatibility

(defalias 'make-local-hook 'ignore)


;;; Convenience aliases

(defalias 'bb   'bury-buffer)
(defalias 'dbf  'delete-file-and-buffer)
(defalias 'dfb  'delete-file-and-buffer)
(defalias 'hex  'hexl-mode)
(defalias 'hff  'hexl-find-file)
(defalias 'kb   'kill-buffer)
(defalias 'plp  'package-list-packages)
(defalias 'qr   'query-replace)
(defalias 'qrr  'query-replace-regexp)
(defalias 'rbf  'rename-file-and-buffer)
(defalias 'rfb  'rename-file-and-buffer)
(defalias 'cal 'calendar)

;;; Set variables

(custom-set-variables
 `(abbrev-file-name (f-join spacemacs-cache-directory "abbrev_defs"))
 `(backup-directory-alist '((".*" . ,(f-join spacemacs-autosaves-directory))))
 `(bookmark-default-file (f-join spacemacs-cache-directory "bookmarks"))
 `(comint-prompt-read-only t)
 `(confirm-nonexistent-file-or-buffer  nil)
 `(default-input-method "TeX")
 `(delete-by-moving-to-trash nil)
 `(delete-old-versions t)
 `(fill-column 80)
 `(initial-major-mode 'org-mode)
 `(kept-new-versions 6)
 `(require-final-newline t)
 `(sentence-end-double-space nil)
 `(tab-width 4)
 `(x-select-enable-clipboard t)
 )

;;; Colours

(defvar solarized-hl-yellow    "#b58900")
(defvar solarized-hl-orange    "#cb4b16")
(defvar solarized-hl-red       "#dc322f")
(defvar solarized-hl-magenta   "#d33682")
(defvar solarized-hl-violet    "#6c71c4")
(defvar solarized-hl-blue      "#268bd2")
(defvar solarized-hl-cyan      "#2aa198")
(defvar solarized-hl-green     "#859900")


;;; Custom faces

(defface core/bg-flash
  '((((class color) (background light))
     :background "darkseagreen2")
    (((class color) (background dark))
     :background "royalblue4"))
  "Face for flashing with a green background."
  :group 'cb-faces)

(defface core/bg-flash-red
  '((t (:background "rosybrown1")))
  "Face for flashing with a red background."
  :group 'cb-faces)
