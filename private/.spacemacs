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
                                     flycheck
                                     projectile
                                     ack
                                     cb-core
                                     cb-org
                                     cb-elisp
                                     cb-cosmetic
                                     cb-git
                                     cb-yasnippet
                                     cb-flycheck
                                     cb-projectile
                                     cb-smartparens
                                     cb-iedit
                                     cb-ledger
                                     )
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(rainbow-delimiters
                                  )
)

;; Settings
;; --------

(setq-default
 ;; Default theme applied at startup
 dotspacemacs-default-theme 'solarized-light
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
)

;; Initialization Hooks
;; --------------------

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
)

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (require 'personal-config nil t)
)

;; Custom variables
;; ----------------

;; Do not write anything in this section. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(iedit-occurrence ((t (:background "#cb4b16" :foreground "white"))))
 '(org-block-background ((((background light)) :background "#f8f1dc") (((background dark)) :background "#11303b")))
 '(org-block-begin-line ((((background light)) :italic t :foreground "#2aa198") (((background dark)) :italic t :foreground "#2aa198")) t)
 '(org-block-end-line ((((background light)) :italic t :foreground "#2aa198") (((background dark)) :italic t :foreground "#2aa198")) t)
 '(org-document-info-keyword ((t :foreground unspecified :inherit org-meta-line)))
 '(org-hide ((t :background unspecified)))
 '(org-level-1 ((t :font "SourceCodePro")))
 '(org-level-2 ((t :font "SourceCodePro")))
 '(org-level-3 ((t :font "SourceCodePro")))
 '(org-level-4 ((t :font "SourceCodePro")))
 '(org-level-5 ((t :font "SourceCodePro")))
 '(org-level-6 ((t :font "SourceCodePro")))
 '(org-level-7 ((t :font "SourceCodePro")))
 '(org-level-8 ((t :font "SourceCodePro")))
 '(org-meta-line ((t :italic nil :inherit font-lock-comment-face)))
 '(parenthesis ((((background light)) :foreground "grey80") (((background dark)) :foreground "#505070")))
 '(yas-field-highlight-face ((((background light)) :background "lightgreen") (((background dark)) :background "green4" :foreground "grey80"))))
