(defvar cb-core-packages
  '(
    ;; package cores go here
    dash
    dash-functional
    s
    f
    noflet
    evil
    evil-surround
    company
    autorevert
    hideshow
    helm
    smex
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

      (bind-keys
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
      (custom-set-faces
       '(helm-selection
         ((((background light)) :background "gray90" :foreground "black" :underline nil)
          (((background dark))  :background "black"  :foreground "white" :underline nil)))))))

(defun cb-core/init-smex ()
  (use-package smex
    :config
    (progn
      (evil-leader/set-key ":" 'smex)
      (bind-key* "M-x" 'smex)
      (bind-key* "S-SPC" 'smex))))
