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
    (custom-set-variables
     '(company-minimum-prefix-length 3))))

(defun cb-core/init-evil ()
  (use-package evil
    :init nil
    :config
    (custom-set-variables
     '(evil-want-visual-char-semi-exclusive t)
     '(evil-shift-width 2)
     '(evil-symbol-word-search 'symbol))))

(defun cb-core/init-evil-surround ()
  (use-package evil-surround
    :config
    (progn
      (custom-set-variables
       '(evil-surround-pairs-alist '((?\( . ("(" . ")"))
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
                                     (?f . surround-function))))

      (add-hook 'emacs-lisp-mode-hook 'core/config-elisp-surround-pairs))))
