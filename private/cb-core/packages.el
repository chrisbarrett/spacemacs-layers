(defvar cb-core-packages
  '(
    ;; package cores go here
    dash
    dash-functional
    s
    f
    noflet
    diminish
    evil
    evil-surround
    company
    autorevert
    hideshow
    helm
    use-package
    aggressive-indent
    relative-line-numbers
    company-quickhelp
    wgrep-ag
    workgroups2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-core-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-core/init-<package-core>
;;
;; (defun cb-core/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-core/init-use-package ()
  (require 'use-package))

(defun cb-core/init-s ()
  (use-package s
    :config (require 's)))

(defun cb-core/init-noflet ()
  (use-package noflet
    :config (require 'noflet)))

(defun cb-core/init-dash ()
  (use-package dash
    :config (require 'dash)))

(defun cb-core/init-dash-functional ()
  (use-package dash-functional
    :config (require 'dash-functional)))

(defun cb-core/init-f ()
  (use-package f
    :config (require 'f)))

(defun cb-core/init-diminish ()
  (use-package diminish
    :config
    (progn
      (require 'diminish)
      (diminish 'auto-fill-function " ≣"))))

(defun cb-core/init-company ()
  (use-package company
    :config
    (setq company-minimum-prefix-length 3)))

(defun cb-core/init-evil ()
  (use-package evil
    :init nil
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

(defun cb-core/init-evil-surround ()
  (use-package evil-surround
    :config
    (progn
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
                      (?f . surround-function)))

      (add-hook 'emacs-lisp-mode-hook 'core/config-elisp-surround-pairs))))

(defun cb-core/init-autorevert ()
  (use-package autorevert
    :diminish auto-revert-mode))

(defun cb-core/init-hideshow ()
  (use-package hideshow
    :diminish hs-minor-mode))

(defun cb-core/init-helm ()
  (use-package helm
    :config
    (progn
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
          (((background dark))  :background "black"  :foreground "white" :underline nil)))))))

(defun cb-core/init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode)
      (global-aggressive-indent-mode))))


(defun cb-core/init-company-quickhelp ()
  (use-package company-quickhelp
    :init
    (add-hook 'company-mode-hook (lambda () (company-quickhelp-mode +1)))
    :config
    (setq company-quickhelp-delay 0.2)))

(defun cb-core/init-wgrep-ag ()
  (use-package wgrep-ag
    :defer t))

(defun cb-core/init-workgroups2 ()
  (use-package workgroups2
    :config
    (progn
      (setq wg-prefix-key (kbd "C-c b"))
      (setq wg-session-file (f-join user-emacs-directory "workgroups-sessions"))
      (setq wg-first-wg-name "default")
      (add-hook 'after-init-hook 'workgroups-mode)
      (add-hook 'workgroups-mode-hook
                (lambda () (diminish 'workgroups-mode "Ⓦ")))

      (define-key workgroups-mode-map (kbd "C-c b s") 'wg-save-session)
      (define-key workgroups-mode-map (kbd "C-c b l") 'wg-load-last-workgroup)
      (define-key workgroups-mode-map (kbd "C-c b ,") 'wg-rename-workgroup)
      (define-key workgroups-mode-map (kbd "C-c b N") 'wg-switch-to-workgroup-left))))
