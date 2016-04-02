;; Menu-bar looks acceptable in OS X. Otherwise it adds clutter.
(when (fboundp 'menu-bar-mode)
  (if (and (eq system-type 'darwin)
           (display-graphic-p))
      (menu-bar-mode +1)
    (menu-bar-mode -1)))

(when (boundp 'window-numbering-mode)
  (window-numbering-mode -1))

(defvar spacemacs-autosaves-directory (concat user-emacs-directory "autosaves/"))

;;; Compatibility

(defalias 'make-local-hook 'ignore)


;;; Convenience aliases

(defalias 'bb  'bury-buffer)
(defalias 'hex 'hexl-mode)
(defalias 'hff 'hexl-find-file)
(defalias 'plp 'paradox-list-packages)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'cal 'calendar)
(defalias 'rfb 'cb-core-rename-file-and-buffer)
(defalias 'rbf 'cb-core-rename-file-and-buffer)
(defalias 'mv  'cb-core-move-file)


;;; Set variables

(setq abbrev-file-name (concat spacemacs-cache-directory "abbrev_defs"))
(setq backup-directory-alist `((".*" . ,spacemacs-autosaves-directory)))
(setq version-control t)
(setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks"))
(setq comint-prompt-read-only t)
(setq confirm-nonexistent-file-or-buffer  nil)
(setq default-input-method "TeX")
(setq delete-by-moving-to-trash nil)
(setq delete-old-versions t)
(setq initial-major-mode 'org-mode)
(setq kept-new-versions 6)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq x-select-enable-clipboard t)
(setq compilation-scroll-output 'first-error)
(setq mac-pass-control-to-system nil)

(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default evil-shift-width 2)

(add-hook 'compilation-filter-hook 'cb-core-ansi-colourise-compilation)

;;; Colours

(defvar solarized-hl-yellow    "#b58900")
(defvar solarized-hl-orange    "#cb4b16")
(defvar solarized-hl-red       "#dc322f")
(defvar solarized-hl-magenta   "#d33682")
(defvar solarized-hl-violet    "#6c71c4")
(defvar solarized-hl-blue      "#268bd2")
(defvar solarized-hl-cyan      "#2aa198")
(defvar solarized-hl-green     "#859900")

;;; Customise spacemacs face.

(custom-set-faces
 '(font-lock-comment-face ((t (:background nil :foreground "#93a1a1"))))
 '(font-lock-variable-name-face ((t (:italic t))))
 '(font-lock-keyword-face ((t (:bold nil)))))

;;; Compat with spacemacs' face remapping.

(defvar core/face-remapping-alist face-remapping-alist
  "Hacky work-around to prevent spacemacs from killing face remaps.")

(defun core/remap-face (from to)
  "Add a face remapping from FROM to TO.
Work around spacemacs' aggressive manipulation of `face-remapping-alist'."
  (add-to-list 'core/face-remapping-alist (cons from to))
  (add-to-list 'face-remapping-alist (cons from to)))

(core/remap-face 'flymake-errline 'flycheck-error)
(core/remap-face 'flymake-warnling 'flycheck-warning)

(defadvice spacemacs//ido-navigation-ms-on-exit (after restore-face-remappings activate)
  (setq face-remapping-alist (-concat face-remapping-alist core/face-remapping-alist)))

(defadvice spacemacs//ido-setup (after restore-face-remappings activate)
  (setq face-remapping-alist (-concat face-remapping-alist core/face-remapping-alist)))

(defadvice spacemacs//helm-before-initialize (after restore-face-remappings activate)
  (setq face-remapping-alist (-concat face-remapping-alist core/face-remapping-alist)))

;;; Custom faces

(defface core/bg-hl-ok
  '((((background dark))  :background "#01304b")
    (((background light)) :background "#e9f2c5"))
  "Face for highlighting regions that represent an 'OK' state."
  :group 'cb-faces)

(defface core/bg-hl-template
  '((((background dark))  :background "#3f4d91")
    (((background light)) :background "thistle"))
  "Face for active template fields."
  :group 'cb-faces)

(defface core/bg-hl-red
  '((((background dark))  :background "#51202b")
    (((background light)) :background "#fee8e5"))
  "Face for highlighting regions that represent an error state."
  :group 'cb-faces)

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

;;; Saving behaviour

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Editing advice

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (let ((dir (f-dirname (buffer-file-name)))
        (file (buffer-file-name)))
    (unless (and file (f-writable? file))
      (when (and (f-exists? dir) (not (f-writable? dir)))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Do not prompt for confirmation for active processes."
  (noflet ((process-list () nil))
    ad-do-it))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Use the current buffer's tab settings when cleaning whitespace."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(defadvice comment-indent-new-line (after add-space activate)
  "Insert a leading space after comment start for new comment lines."
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(defadvice insert-for-yank (after clean-whitespace)
  "Clean up whitespace when inserting yanked text."
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

;;; Language modes

(add-to-list 'auto-mode-alist (cons (rx ".zsh" eos) 'shell-script-mode))


;;; Highlight TODO keywords in all modes.

(defun core/set-todo-font-lock ()
  "Highlight todos in programming language modes."
  (font-lock-add-keywords
   nil `((,(rx (or "//" (syntax comment-start)) (* nonl) bow (group (or "FIX" "TODO" "FIXME" "HACK" "REFACTOR")) ":")
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'core/set-todo-font-lock)

;;; Hide DOS EOL

(defun core/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook 'core/hide-dos-eol)



;;; Misc config

(add-hook 'diff-auto-refine-mode-hook (lambda () (aggressive-indent-mode -1)))

(defadvice dotspacemacs/sync-configuration-layers (after font-lock-fontify activate)
  "Apply font-lock to buffer after reloading Spacemacs."
  (font-lock-fontify-buffer))

(defadvice spacemacs/cycle-spacemacs-theme (after font-lock-fontify-all activate)
  "Reapply font lock for all buffers."
  (--each (buffer-list) (with-current-buffer it
                          (when font-lock-mode
                            (font-lock-fontify-buffer)))))


;;: Show time in the modeline

(setq display-time-default-load-average nil)
(display-time-mode +1)
