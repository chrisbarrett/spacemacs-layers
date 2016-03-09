;;; extensions.el --- cb-evil Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-evil-packages
  '(evil
    evil-surround
    evil-numbers))

;;; HACK: work around keymap definition issue.
(defun bind-map-evil-define-key (&rest _))

(defun cb-evil/post-init-evil ()
  (use-package evil
    :config
    (progn
      (setq evil-want-visual-char-semi-exclusive t)
      (setq evil-shift-width 2)
      (setq evil-symbol-word-search 'symbol)

      (evil-global-set-key 'normal (kbd "\\") 'evil-repeat-find-char-reverse)

      ;; Make window management work for all modes

      (bind-keys*
       :prefix "C-w"
       :prefix-map evil/window-emu
       ("C-w" . evil-window-prev)
       ("C-s" . split-window-vertically)
       ("C-v" . split-window-horizontally)
       ("C-o" . delete-other-windows)
       ("C-c" . delete-window)
       ("w" . evil-window-prev)
       ("s" . split-window-vertically)
       ("v" . split-window-horizontally)
       ("o" . delete-other-windows)
       ("c" . delete-window)))))

(defun cb-evil/post-init-evil-surround ()
  (use-package evil-surround
    :config
    (setq-default evil-surround-pairs-alist
                  '((?\( . ("(" . ")"))
                    (?\[ . ("[" . "]"))
                    (?\{ . ("{" . "}"))

                    (?\) . ("(" . ")"))
                    (?\] . ("[" . "]"))
                    (?\} . ("{" . "}"))

                    (?# . ("#{" . "}"))
                    (?b . ("(" . ")"))
                    (?B . ("{" . "}"))
                    (?> . ("<" . ">"))
                    (?t . surround-read-tag)
                    (?< . surround-read-tag)
                    (?f . surround-function)))))

(defun cb-evil/post-init-evil-numbers ()
  (use-package evil-numbers
    :config
    (progn
      (evil-global-set-key 'normal (kbd "+") 'spacemacs/evil-numbers-increase)
      (evil-global-set-key 'normal (kbd "-") 'spacemacs/evil-numbers-decrease))))
