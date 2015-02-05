;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Configuration Layers
;; --------------------

(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of configuration layers to load.
 dotspacemacs-configuration-layers '(company-mode
                                     ;; smex
                                     auctex
                                     haskell
                                     osx
                                     git
                                     scala
                                     cb-core
                                     cb-org
                                     cb-elisp
                                     cb-cosmetic
                                     cb-spelling
                                     cb-git
                                     cb-yasnippet
                                     cb-project
                                     cb-smartparens
                                     cb-ledger
                                     cb-proof
                                     cb-haskell
                                     cb-scala
                                     cb-csharp
                                     cb-rust
                                     cb-idris
                                     cb-coffeescript
                                     )
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(rainbow-delimiters
                                  )
 )

;; Settings
;; --------

(setq-default
 ;; Default theme applied at startup
 dotspacemacs-themes '(solarized-light solarized-dark)
 ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
 ;; the commands bound to the current keystrokes.
 dotspacemacs-guide-key-delay 0.4
 ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only)
 dotspacemacs-fullscreen-at-startup nil
 ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
 ;; overrides the default behavior of Emacs which recenters the point when
 ;; it reaches the top or bottom of the screen
 dotspacemacs-smooth-scrolling t
 ;; If non nil pressing 'jk' in insert state, ido or helm will activate the
 ;; evil leader.
 dotspacemacs-feature-toggle-leader-on-jk nil
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository nil
 ;; Startup banner
 dotspacemacs-startup-banner 'doge
 )

;; Initialization Hooks
;; --------------------

(defun core/install-package (pkg)
  (require 'paradox nil t)
  (cond
    ((featurep 'paradox)
     (paradox-require pkg))
    ((package-installed-p pkg)
     (require pkg))
    (t
     (package-install pkg)
     (require pkg)))

  (eval-after-load 'paradox
   `(paradox-require ',pkg)))

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)

  (core/install-package 'noflet)
  (core/install-package 's)
  (core/install-package 'dash)
  (core/install-package 'dash-functional)
  (core/install-package 'helm)
  (require 'helm-mode)

  (setq-default git-enable-github-support t)
  (add-to-list 'exec-path "~/.cabal/bin/")
  (add-to-list 'exec-path "~/bin/")
  (load (concat user-emacs-directory "private/cb-core/funcs.el"))
  (load (concat user-emacs-directory "private/cb-core/config.el")))

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  (require 'personal-config nil t))
