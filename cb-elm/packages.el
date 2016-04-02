;;; packages.el --- cb-elm layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cb-elm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cb-elm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cb-elm/pre-init-PACKAGE' and/or
;;   `cb-elm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(require 'dash nil t)
(require 'use-package nil t)
(eval-when-compile
  (require 'smartparens nil t))

(autoload 'evil-define-key "evil-core")

(defconst cb-elm-packages
  '(autoinsert
    aggressive-indent
    elm-mode
    smart-ops
    smartparens
    (elm-insert-exposing :location local)
    (elm-meta-ret :location local))
  "The list of Lisp packages required by the cb-elm layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun cb-elm/post-init-autoinsert ()
  (use-package autoinsert
    :config
    (add-to-list 'auto-insert-alist
                 '(("\\.elm\\'" . "Elm Src File")
                   nil
                   "module " (s-upper-camel-case (file-name-base)) " where" "\n"
                   "\n"
                   _
                   "\n"
                   "-- Model\n"
                   "\n"
                   "-- Update\n"
                   "\n"
                   "-- View\n"
                   "\n"
                   ))))

(defun cb-elm/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (add-to-list 'aggressive-indent-excluded-modes 'elm-mode)))

(defun cb-elm/post-init-elm-mode ()
  (use-package elm-mode
    :defer t
    :config
    (progn
      (define-key elm-mode-map (kbd "RET") #'newline)
      (evil-define-key 'insert elm-mode-map (kbd "RET") #'newline))))

(defun cb-elm/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (let ((ops
           (-flatten-n 1
                       (list
                        (smart-ops "->" "=>")
                        (smart-ops "$" "=" "~" "^" ":" "?")
                        (smart-ops "," :pad-before nil :pad-after t)
                        (smart-ops-default-ops)))))
      (define-smart-ops-for-mode 'elm-mode ops)
      (define-smart-ops-for-mode 'elm-interactive-mode ops))))

(defun cb-elm/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (sp-with-modes '(elm-mode elm-interactive-mode)
      (sp-local-pair "\"" "\"" :post-handlers '(:add sp-external-padding))
      (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding))
      (sp-local-pair "(" ")" :post-handlers '(:add sp-external-padding))
      (sp-local-pair "[" "]" :post-handlers '(:add sp-internal-and-external-padding))
      (sp-local-pair "`" "`" :post-handlers '(:add sp-external-padding))
      (sp-local-pair "'" "'" :actions '(:rem insert)))))

(defun cb-elm/init-elm-insert-exposing ()
  (use-package elm-insert-exposing
    :commands elm-insert-exposing-init
    :config
    (add-hook 'elm-mode-hook #'elm-insert-exposing-init)))

(defun cb-elm/init-elm-meta-ret ()
  (use-package elm-meta-ret
    :commands elm-meta-ret-init
    :init
    (add-hook 'elm-mode-hook #'elm-meta-ret-init)))

;;; packages.el ends here
