;;; extensions.el --- cb-evil Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'evil nil t)
  (require 'cb-use-package-extensions)
  (require 'use-package))

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

    :bind
    (("<f2>" . next-multiframe-window)
     ("S-<f2>" . previous-multiframe-window))

    :evil-bind
    (:state
     normal
     ("\\" . evil-repeat-find-char-reverse)
     ("C-w -" . split-window-below)
     ("C-w /" . evil-window-vsplit)
     ("C-w k" . next-multiframe-window)
     ("C-w j" . previous-multiframe-window)

     :state emacs
     ("C-w -" . next-multiframe-window)
     ("C-w /" . evil-window-vsplit)
     ("C-w k" . next-multiframe-window)
     ("C-w j" . previous-multiframe-window))

    :leader-bind
    (("wo" . delete-other-windows))

    :config
    (progn
      (setq evil-want-visual-char-semi-exclusive t)
      (setq evil-shift-width 2)
      (setq evil-symbol-word-search 'symbol)

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
    :evil-bind
    (:state
     normal
     ("+" . evil-numbers/inc-at-pt)
     ("-" . evil-numbers/dec-at-pt))))

(defun cb-evil/init-cb-evil-visual-defaults ()
  (use-package cb-evil-visual-defaults
    :commands cb-evil-visual-defaults-init
    :config (cb-evil-visual-defaults-init)))

(provide 'packages)

;;; packages.el ends here
