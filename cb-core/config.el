;;; config.el --- Basic configuration options.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (boundp 'window-numbering-mode)
  (window-numbering-mode -1))

(defvar spacemacs-autosaves-directory (concat user-emacs-directory "autosaves/"))

;; Show file or buffer name in the title bar.

(defun core/frame-title-string ()
  (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name))
    (buffer-name)))

(setq frame-title-format `(:eval (core/frame-title-string)))

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

;;; Set variables

(setq abbrev-file-name (concat (with-no-warnings spacemacs-cache-directory) "abbrev_defs"))
(setq backup-directory-alist `((".*" . ,spacemacs-autosaves-directory)))
(setq version-control t)

(with-no-warnings (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")))
(with-no-warnings (setq comint-prompt-read-only t))
(setq confirm-nonexistent-file-or-buffer  nil)
(setq default-input-method "TeX")
(setq delete-by-moving-to-trash nil)
(setq delete-old-versions t)
(setq initial-major-mode 'org-mode)
(setq kept-new-versions 6)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq x-select-enable-clipboard t)
(with-no-warnings (setq compilation-scroll-output 'first-error))
(setq mac-pass-control-to-system nil)

(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default evil-shift-width 2)

;;; Customise spacemacs face.

(custom-set-faces
 '(font-lock-comment-face ((t (:background nil :foreground "#93a1a1"))))
 '(font-lock-variable-name-face ((t (:italic t))))
 '(font-lock-keyword-face ((t (:bold nil)))))

;;; Saving behaviour

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Editing advice

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

;;; Hide DOS EOL

(defun core/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook 'core/hide-dos-eol)



;;; Misc config

(defun cb-core--font-lock-all-buffers (&rest _)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when font-lock-mode
        (font-lock-fontify-buffer)))))

(advice-add 'spacemacs/cycle-spacemacs-theme :after #'cb-core--font-lock-all-buffers)
(advice-add 'dotspacemacs/sync-configuration-layers :after #'cb-core--font-lock-all-buffers)

;;; config.el ends here
