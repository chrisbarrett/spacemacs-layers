;;; packages.el --- cb-company layer packages file for Spacemacs.
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
;; added to `cb-company-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cb-company/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cb-company/pre-init-PACKAGE' and/or
;;   `cb-company/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-company-packages
  '(company)
  "The list of Lisp packages required by the cb-company layer.

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

(defun cb-company/post-init-company ()
  (use-package company
    :config
    (progn
      (dolist (map (list company-active-map company-search-map company-filter-map))
        (define-key map (kbd "C-n") 'company-select-next)
        (define-key map (kbd "C-p") 'company-select-previous)
        (define-key map (kbd "C-h") 'company-show-doc-buffer)
        (define-key map (kbd "C-w") nil))

      (defun cb-company--set-company-vars ()
        (setq company-minimum-prefix-length 3)
        (setq company-tooltip-align-annotations t))

      (add-hook 'company-mode-hook #'cb-company--set-company-vars))))

;;; packages.el ends here
