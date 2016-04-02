;;; extensions.el --- cb-evil Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'evil nil t)
  (require 'use-package nil t))

(evil-ex-define-cmd "nospell"
                    (lambda ()
                      (interactive)
                      (turn-off-flyspell)))

(evil-ex-define-cmd "spell"
                    (lambda ()
                      (interactive)
                      (turn-on-flyspell)))

(defconst cb-evil-packages
  '(evil
    evil-surround
    evil-numbers
    (cb-evil-visual-defaults :location local)))

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

      ;; Window management

      (spacemacs/set-leader-keys "wo" 'delete-other-windows)
      (spacemacs/set-leader-keys "|" 'cb-core-toggle-window-split)

      (evil-global-set-key 'normal (kbd "C-w |") 'cb-core-toggle-window-split)
      (evil-global-set-key 'normal (kbd "C-w -") 'split-window-below)
      (evil-global-set-key 'normal (kbd "C-w /") 'evil-window-vsplit)
      (evil-global-set-key 'emacs (kbd "C-w -") 'next-multiframe-window)
      (evil-global-set-key 'emacs (kbd "C-w /") 'evil-window-vsplit)

      ;; Frame navigation

      (global-set-key (kbd "<f2>") 'next-multiframe-window)
      (global-set-key (kbd "S-<f2>") 'previous-multiframe-window)

      (evil-global-set-key 'normal (kbd "C-w k") 'next-multiframe-window)
      (evil-global-set-key 'normal (kbd "C-w j") 'previous-multiframe-window)
      (evil-global-set-key 'emacs (kbd "C-w k") 'next-multiframe-window)
      (evil-global-set-key 'emacs (kbd "C-w j") 'previous-multiframe-window)


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

(defun cb-evil/init-cb-evil-visual-defaults ()
  (use-package cb-evil-visual-defaults
    :commands cb-evil-visual-defaults-init
    :config (cb-evil-visual-defaults-init)))

(provide 'packages)

;;; packages.el ends here
