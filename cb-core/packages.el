;;; packages.el --- cb-core Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cb-vars nil t))

(require 'use-package)
(require 's)
(require 'dash)
(require 'f)
(require 'noflet)

(defconst cb-core-packages
  '(dash-functional
    diminish
    auto-revert
    hideshow
    helm
    aggressive-indent
    ag
    wgrep-ag
    alert
    helm-gtags
    world-time-mode
    iedit
    hl-line
    eldoc
    ido
    recentf
    neotree
    time

    (cb-buffers :location local)
    (locate-key-binding :location local)
    (smart-ops :location local)
    (case :location local)
    (create-layer-local-package :location local)
    (indirect-region :location local)
    (remove-line-breaks :location local)
    (insert-timestamp :location local)
    (insert-shebang :location local)
    (insert-variable-value :location local)
    (insert-guid :location local)
    (helm-http-status :location local)
    (indent-dwim :location local)))

(defun cb-core/user-config ()
  "This procedure should be called in `dotspacemacs/user-config'."
  (setq recentf-max-saved-items 1000)
  (setq bookmark-save-flag nil)

  ;; Set ace-window keys for dvorak.
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)))

(defun cb-core/init-dash-functional ()
  (use-package dash-functional
    :config (require 'dash-functional)))

(defun cb-core/post-init-diminish ()
  (diminish 'auto-fill-function " ≣"))

(defun cb-core/post-init-autorevert ()
  (diminish 'auto-revert-mode))

(defun cb-core/post-init-hideshow ()
  (diminish 'hs-minor-mode))

(defun cb-core/post-init-helm ()
  (require 'helm)
  (helm-autoresize-mode +1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)

  (setq helm-locate-command
        (pcase system-type
          (`gnu/linux "locate -i -r %s")
          (`berkeley-unix "locate -i %s")
          (`windows-nt "es %s")
          (`darwin "mdfind -name %s %s | egrep -v '/Library/(Caches|Mail)/'")
          (t "locate %s")))

  (custom-set-faces
   `(helm-locate-finish
     ((t (:foreground ,solarized-hl-cyan))))
   '(helm-selection
     ((((background light)) :background "gray90" :foreground "black" :underline nil)
      (((background dark))  :background "black"  :foreground "white" :underline nil)))))

(defun cb-core/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
      (global-aggressive-indent-mode))))

(defun cb-core/init-ag ()
  (use-package ag :commands ag))

(defun cb-core/init-wgrep-ag ()
  (use-package wgrep-ag
    :defer t))

(defun cb-core/init-alert ()
  (use-package alert
    :defer t
    :config
    (setq alert-default-style 'message)))

(defun cb-core/post-init-helm-gtags ()
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-pulse-at-cursor t)
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-suggested-key-mapping t)

  (with-eval-after-load 'pulse
    (core/remap-face 'pulse-highlight-face 'cb-faces-bg-flash)
    (core/remap-face 'pulse-highlight-start-face 'cb-faces-bg-flash))

  (with-eval-after-load 'helm-gtags
    (dolist (state '(normal insert))
      (evil-define-key state helm-gtags-mode-map
        (kbd "M-.") 'helm-gtags-dwim
        (kbd "M-,") 'helm-gtags-pop-stack))

    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

(defun cb-core/init-world-time-mode ()
  (use-package world-time-mode
    :commands world-time-list
    :init
    (spacemacs/set-leader-keys "at" 'world-time-list)
    :config
    (progn
      (setq display-time-world-list '(("Pacific/Auckland" "NZT")
                                      ("UTC" "UTC")
                                      ("Europe/Berlin" "Germany")
                                      ("America/Los_Angeles" "Los Angeles")
                                      ("America/New_York" "New York")
                                      ("Australia/Sydney" "Sydney")))

      (evil-define-key 'normal world-time-table-mode-map (kbd "q") 'quit-window)
      (add-hook 'world-time-table-mode-hook 'hl-line-mode))))

(defun cb-core/init-smart-ops ()
  (use-package smart-ops
    :diminish smart-ops-mode
    :functions (smart-ops-init)
    :config (smart-ops-init)))

(defun cb-core/post-init-iedit ()
  (custom-set-faces
   `(iedit-occurrence ((t (:background ,solarized-hl-orange :foreground "white"))))))

(defun cb-core/post-init-hl-line ()
  (global-hl-line-mode -1))

(defun cb-core/post-init-eldoc ()
  (setq eldoc-idle-delay 0.1))

(defun cb-core/init-case ()
  (use-package case))

(defun cb-core/init-locate-key-binding ()
  (use-package locate-key-binding
    :commands (locate-key-binding)))

(defun cb-core/post-init-recentf ()
  (use-package recentf
    :config
    (progn
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
      (setq recentf-max-menu-items 10)
      (setq recentf-keep '(file-remote-p file-readable-p))

      (defun cb-core/shut-up-recentf (old-fn &rest rs)
        (noflet ((message (&rest args)))
          (apply old-fn rs)))

      (advice-add 'recentf-cleanup :around #'cb-core/shut-up-recentf)

      (setq recentf-exclude
            (-distinct (-concat recentf-exclude
                                (cb-core-regexp-quoted-ignored-dirs)
                                cb-vars-ignored-files-regexps)))
      (recentf-cleanup))))

(defun cb-core/post-init-ido ()
  (setq ido-use-filename-at-point 'guess)
  (add-to-list 'ido-ignore-buffers "\\*helm.*")
  (add-to-list 'ido-ignore-buffers "\\*Minibuf.*")
  (dolist (regexp cb-vars-ignored-files-regexps)
    (add-to-list 'ido-ignore-files regexp)))

(defun cb-core/init-cb-buffers ()
  (use-package cb-buffers
    :config
    (progn
      (bind-key* (kbd "C-<backspace>") 'cb-buffers-maybe-kill)
      (bind-key (kbd "C-c k b") 'cb-buffers-maybe-kill-all))))

(defun cb-core/post-init-neotree ()
  (use-package neotree
    :config
    (setq neo-theme 'arrow)))

(defun cb-core/init-create-layer-local-package ()
  (use-package create-layer-local-package
    :commands (create-layer-local-package)))

(defun cb-core/init-indirect-region ()
  (use-package indirect-region
    :commands (indirect-region)))

(defun cb-core/init-remove-line-breaks ()
  (use-package remove-line-breaks
    :commands (remove-line-breaks)))

(defun cb-core/init-insert-timestamp ()
  (use-package insert-timestamp
    :commands (insert-timestamp)
    :init (spacemacs/set-leader-keys "iT" #'insert-timestamp)))

(defun cb-core/init-insert-shebang ()
  (use-package insert-shebang
    :commands (insert-shebang)
    :init (spacemacs/set-leader-keys "i#" 'insert-shebang)))

(defun cb-core/init-insert-variable-value ()
  (use-package insert-variable-value
    :commands (insert-variable-value)))

(defun cb-core/init-insert-guid ()
  (use-package insert-guid
    :commands (insert-guid)
    :init (spacemacs/set-leader-keys "iU" 'insert-guid)))

(defun cb-core/init-helm-http-status ()
  (use-package helm-http-status
    :commands (helm-http-status)))

(defun cb-core/init-indent-dwim ()
  (use-package indent-dwim
    :functions (indent-dwim-init)
    :config (indent-dwim-init)))

(defun cb-core/init-time ()
  (use-package time
    :config
    (progn
      (setq display-time-default-load-average nil)
      (display-time-mode +1))))

;;; packages.el ends here
