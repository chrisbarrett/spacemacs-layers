;;; packages.el --- cb-circe layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Barrett <chris.d.barrett@me.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cb-circe-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cb-circe/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another spacemacs layer, so
;;   define the functions `cb-circe/pre-init-PACKAGE' and/or
;;   `cb-circe/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(require 'use-package)
(require 'dash)

(defconst cb-circe-packages
  '(circe
    persp-mode
    circe-notifications
    helm
    (circe-show-channels :location local)))

(defun cb-circe/init-circe ()
  (use-package circe
    :commands circe
    :config
    (progn
      (set-face-background 'circe-prompt-face nil)
      (set-face-foreground 'circe-prompt-face cb-vars-solarized-hl-magenta)

      (defface cb-circe-self-say-face
        `((t (:weight bold :foreground ,cb-vars-solarized-hl-blue)))
        "The face for the Circe prompt.")

      (setq circe-reduce-lurker-spam t)
      (setq circe-use-cycle-completion t)
      (setq circe-active-users-timeout (* 60 30)) ; 30 minutes
      (setq circe-format-say "{nick}> {body}")
      (setq circe-format-self-say (concat (propertize ">>>" 'face 'cb-circe-self-say-face) " {body}"))
      (setq circe-prompt-string (concat (propertize ">>>" 'face 'circe-prompt-face) " "))
      (setq circe-highlight-nick-type 'all)

      ;; Show channel name in prompt.

      (defun cb-circe/bufname-to-channame (s)
        (-let* (((_ gitter-fmt) (s-match (rx (+ nonl) "/" (group (+ nonl)) eos) s))
                ((_ standard-fmt) (s-match (rx "#" (group (+ nonl)) eos) s)))
          (or gitter-fmt standard-fmt)))

      (defun cb-circe/set-prompt ()
        (let* ((chan (cb-circe/bufname-to-channame (buffer-name)))
               (prompt (concat "#" (propertize chan 'face 'circe-prompt-face) " > ")))
          (lui-set-prompt prompt)))

      (add-hook 'circe-chat-mode-hook #'cb-circe/set-prompt)

      ;; Timestamps in margins.

      (setq lui-time-stamp-position 'right-margin)
      (setq lui-time-stamp-format "%H:%M")
      (setq lui-fill-type nil)))

  (use-package circe-color-nicks
    :commands enable-circe-color-nicks
    :init
    (with-eval-after-load 'circe
      (enable-circe-color-nicks))
    :config
    (setq circe-color-nicks-everywhere t))

  (use-package lui
    :init
    (progn
      (defun cb-circe/set-local-vars ()
        (setq fringes-outside-margins t)
        (setq right-margin-width 5)
        (setq word-wrap t)
        (setq wrap-prefix "    "))

      (add-hook 'lui-mode-hook #'cb-circe/set-local-vars)))

  (use-package lui-autopaste
    :commands enable-lui-autopaste
    :init
    (add-hook 'circe-channel-mode-hook #'enable-lui-autopaste)))

(defun cb-circe/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (dolist (mode '(circe-channel-mode circe-query-mode circe-server-mode))
      (add-to-list 'helm-mode-no-completion-in-region-in-modes mode))))

(defun cb-circe/init-circe-notifications ()
  (use-package circe-notifications
    :commands enable-circe-notifications
    :after circe
    :init
    (add-hook 'circe-server-connected-hook #'enable-circe-notifications)
    :config
    (progn
      ;;; HACK: Fix broken implementation of internal function.

      (defun circe-notifications-nicks-on-all-networks ()
        "Get a list of all nicks in use according to `circe-network-options'."
        (delete-dups (mapcar (lambda (opt)
                               (plist-get (cdr opt) :nick))
                             circe-network-options)))

      (when (eq system-type 'darwin)
        (setq circe-notifications-alert-style 'osx-notifier)))))

(defun cb-circe/post-init-persp-mode ()
  (defun cb-circe/maybe-refresh-layout ()
    (when (equal "IRC" (spacemacs//current-layout-name))
      (circe-show-channels)))

  (add-hook 'circe-channel-mode-hook #'cb-circe/maybe-refresh-layout)
  (add-hook 'persp-activated-hook #'cb-circe/maybe-refresh-layout)

  (spacemacs|define-custom-layout "IRC"
    :binding "i"
    :body
    (circe-show-channels))

  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (push (lambda (buf)
            (with-current-buffer buf (derived-mode-p 'circe-mode)))
          persp-filter-save-buffers-functions)))

(defun cb-circe/init-circe-show-channels ()
  (use-package circe-show-channels
    :after circe
    :bind
    (:map
     circe-mode-map
     ("C-c C-s" . circe-show-channels))
    :config
    (setq circe-show-channels-priority '(("#haskell" . 1)))))

;;; packages.el ends here
