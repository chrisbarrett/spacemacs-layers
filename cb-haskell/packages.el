;;; packages.el --- cb-haskell layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Barrett <chris.d.barrett@me.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions)
  (require 'use-package nil t))

(defconst cb-haskell-packages
  '(haskell-mode
    indent-dwim
    intero
    llvm-mode
    shm

    (ghc-dump :location local)
    (haskell-flyspell :location local)
    (haskell-ghc-opts :location local)
    (haskell-imports :location local)
    (haskell-pragmas :location local)
    (haskell-ret :location local)
    (haskell-autoinsert :location local)
    (cb-haskell-alignment :location local)
    (haskell-flycheck-holes :location local)
    (cb-haskell-meta-ret :location local)
    (cb-haskell-smart-ops :location local)
    (cb-haskell-ctrl-c-ctrl-c :location local)
    (cb-stack-hoogle :location local)))

(defun cb-haskell/init-haskell-mode ()
  (use-package haskell-mode
    :bind
    (:map
     haskell-mode-map
     ("<backtab>" . haskell-indentation-indent-backwards)
     ("TAB" . haskell-indentation-indent-line)
     ("M-P" . flymake-goto-prev-error)
     ("M-N" . flymake-goto-next-error)
     ("C-," . haskell-move-nested-left)
     ("C-." . haskell-move-nested-right)
     ("C-c C-d" . haskell-w3m-open-haddock)
     ("C-c C-f" . haskell-cabal-visit-file))

    :evil-bind
    (:map
     haskell-mode-map
     :state normal
     ("<backtab>" . haskell-indentation-indent-backwards)
     ("TAB" . haskell-indentation-indent-line))

    :config
    (progn
      (setq haskell-process-type 'stack-ghci)
      (setq haskell-process-use-presentation-mode t)
      (setq haskell-process-show-debug-tips t)
      (setq haskell-stylish-on-save t)

      ;; Use 4 space indentation style.

      (setq haskell-indentation-layout-offset 4)
      (setq haskell-indentation-starter-offset 2)
      (setq haskell-indentation-where-pre-offset 2)
      (setq haskell-indentation-where-post-offset 2)
      (setq haskell-indentation-left-offset 4)
      (setq haskell-indent-spaces 4)

      (defun cb-haskell/set-indentation-step ()
        (with-no-warnings (setq evil-shift-width 4))
        (setq tab-width 4))

      (add-hook 'haskell-mode-hook #'cb-haskell/set-indentation-step)

      ;; Ignore generated files.

      (add-to-list 'completion-ignored-extensions ".hi")
      (add-to-list 'completion-ignored-extensions ".gm")

      ;; Disable some faces.

      (custom-set-faces
       '(haskell-operator-face ((t :italic nil))))))

  (use-package haskell-debug
    :after haskell-mode
    :config
    (progn
      (add-hook 'haskell-debug-mode-hook #'flyspell-mode-off)

      (with-no-warnings
        (evilified-state-evilify-map haskell-debug-mode-map
          :mode haskell-debug-mode
          :bindings
          (kbd "n") #'haskell-debug/next
          (kbd "N") #'haskell-debug/previous
          (kbd "p") #'haskell-debug/previous
          (kbd "q") #'quit-window))))

  (use-package haskell-presentation-mode
    :after haskell-mode
    :evil-bind
    (:state
     normal
     :mode haskell-presentation-mode-map
     ("q" . quit-window))))

(defun cb-haskell/init-intero ()
  (use-package intero
    :init
    (progn
      (defun cb-haskell/maybe-intero-mode ()
        (unless (or (derived-mode-p 'ghc-core-mode)
                    (equal (get major-mode 'mode-class) 'special))
          (intero-mode +1)))
      (add-hook 'haskell-mode-hook #'cb-haskell/maybe-intero-mode))

    :bind
    (:map
     intero-mode-map
     ("M-." . intero-goto-definition)
     ("M-," . pop-tag-mark))

    :evil-bind
    (:map
     intero-mode-map
     :state normal
     ("M-." . intero-goto-definition)
     ("M-," . pop-tag-mark))

    :leader-bind
    (:mode
     haskell-mode
     ("t" . intero-targets))))

(defun cb-haskell/init-shm ()
  ;; Only use SHM for buffer parsing utilities.
  (use-package shm
    :defer t))

(defun cb-haskell/post-init-indent-dwim ()
  (use-package indent-dwim
    :config
    (add-to-list 'indent-dwim-commands-alist '(haskell-mode . haskell-mode-stylish-buffer))))

(defun cb-haskell/init-llvm-mode ()
  (use-package llvm-mode))

(defun cb-haskell/init-ghc-dump ()
  (use-package ghc-dump
    :after haskell-mode
    :leader-bind (:mode haskell-mode ("d" . ghc-dump-popup))
    :bind (:map ghc-dump-popup-mode-map ("q" . quit-window))
    :config
    (with-eval-after-load 'aggressive-indent
      (add-to-list 'aggressive-indent-excluded-modes 'asm-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'llvm-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'ghc-core-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'ghc-type-dump-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'ghc-stg-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'ghc-cmm-mode))))

(defun cb-haskell/init-haskell-flyspell ()
  (use-package haskell-flyspell
    :functions (haskell-flyspell-init)
    :init (add-hook 'haskell-mode-hook #'haskell-flyspell-init)))

(defun cb-haskell/init-haskell-ghc-opts ()
  (use-package haskell-ghc-opts
    :leader-bind (:mode haskell-mode ("io" . haskell-ghc-opts-insert))))

(defun cb-haskell/init-haskell-imports ()
  (use-package haskell-imports
    :leader-bind (:mode haskell-mode
                        ("ii" . haskell-imports-insert-unqualified)
                        ("iq" . haskell-imports-insert-qualified))))

(defun cb-haskell/init-haskell-pragmas ()
  (use-package haskell-pragmas
    :leader-bind (:mode haskell-mode ("il" . haskell-pragmas-insert))))

(defun cb-haskell/init-haskell-autoinsert ()
  (use-package haskell-autoinsert
    :functions (haskell-autoinsert-init)
    :config (haskell-autoinsert-init)))

(defun cb-haskell/init-cb-haskell-alignment ()
  (use-package cb-haskell-alignment
    :functions cb-haskell-alignment-init
    :config
    (add-hook 'haskell-mode-hook #'cb-haskell-alignment-init)))

(defun cb-haskell/init-haskell-ret ()
  (use-package haskell-ret
    :after haskell-mode
    :evil-bind (:map haskell-mode-map :state insert ("RET" . haskell-ret))))

(defun cb-haskell/init-haskell-flycheck-holes ()
  (use-package haskell-flycheck-holes
    :after (haskell-mode flycheck)
    :config
    (add-hook 'haskell-mode-hook #'haskell-flycheck-holes-init)))

(defun cb-haskell/init-cb-haskell-meta-ret ()
  (use-package cb-haskell-meta-ret
    :after haskell-mode
    :bind (:map haskell-mode-map ("M-RET" . cb-haskell-meta-ret))))

(defun cb-haskell/init-cb-haskell-smart-ops ()
  (use-package cb-haskell-smart-ops
    :after haskell-mode
    :config (cb-haskell-smart-ops-init)))

(defun cb-haskell/init-cb-haskell-ctrl-c-ctrl-c ()
  (use-package cb-haskell-ctrl-c-ctrl-c
    :after haskell-mode
    :bind (:map haskell-mode-map ("C-c C-c" . cb-haskell-ctrl-c-ctrl-c))))

(defun cb-haskell/init-cb-stack-hoogle ()
  (use-package cb-stack-hoogle
    :after haskell-mode
    :bind
    (:map haskell-mode-map ("C-c C-h" . cb-stack-hoogle))
    :evil-bind
    (:map haskell-mode-map :state normal ("RET" . cb-stack-hoogle-info-at-pt))))

;;; packages.el ends here
