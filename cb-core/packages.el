;;; packages.el --- cb-calc Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-core-packages
  '(
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
    aggressive-indent
    company-quickhelp
    ag
    wgrep-ag
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-core-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t)
  (require 's nil t)
  (require 'dash nil t)
  (require 'f nil t)
  )

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
          (((background dark))  :background "black"  :foreground "white" :underline nil))))

      ;; Helm command to display HTTP status codes.

      (defvar helm-httpstatus-source
        '((name . "HTTP STATUS")
          (candidates . (("100 Continue") ("101 Switching Protocols")
                         ("102 Processing") ("200 OK")
                         ("201 Created") ("202 Accepted")
                         ("203 Non-Authoritative Information") ("204 No Content")
                         ("205 Reset Content") ("206 Partial Content")
                         ("207 Multi-Status") ("208 Already Reported")
                         ("300 Multiple Choices") ("301 Moved Permanently")
                         ("302 Found") ("303 See Other")
                         ("304 Not Modified") ("305 Use Proxy")
                         ("307 Temporary Redirect") ("400 Bad Request")
                         ("401 Unauthorized") ("402 Payment Required")
                         ("403 Forbidden") ("404 Not Found")
                         ("405 Method Not Allowed") ("406 Not Acceptable")
                         ("407 Proxy Authentication Required") ("408 Request Timeout")
                         ("409 Conflict") ("410 Gone")
                         ("411 Length Required") ("412 Precondition Failed")
                         ("413 Request Entity Too Large")
                         ("414 Request-URI Too Large")
                         ("415 Unsupported Media Type")
                         ("416 Request Range Not Satisfiable")
                         ("417 Expectation Failed") ("418 I'm a teapot")
                         ("422 Unprocessable Entity") ("423 Locked")
                         ("424 Failed Dependency") ("425 No code")
                         ("426 Upgrade Required") ("428 Precondition Required")
                         ("429 Too Many Requests")
                         ("431 Request Header Fields Too Large")
                         ("449 Retry with") ("500 Internal Server Error")
                         ("501 Not Implemented") ("502 Bad Gateway")
                         ("503 Service Unavailable") ("504 Gateway Timeout")
                         ("505 HTTP Version Not Supported")
                         ("506 Variant Also Negotiates")
                         ("507 Insufficient Storage") ("509 Bandwidth Limit Exceeded")
                         ("510 Not Extended")
                         ("511 Network Authentication Required")))
          (action . message)))

      (defun helm-httpstatus ()
        (interactive)
        (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*")))))

(defun cb-core/init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
      (global-aggressive-indent-mode))))

(defun cb-core/init-company-quickhelp ()
  (use-package company-quickhelp
    :init
    (add-hook 'company-mode-hook (lambda () (company-quickhelp-mode +1)))
    :config
    (setq company-quickhelp-delay 0.2)))

(defun cb-core/init-ag ()
  (use-package ag :commands ag))

(defun cb-core/init-wgrep-ag ()
  (use-package wgrep-ag
    :defer t))
