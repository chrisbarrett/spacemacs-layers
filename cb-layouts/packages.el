;;; packages.el --- cb-layouts layer packages file for Spacemacs.
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
;; added to `cb-layouts-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cb-layouts/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cb-layouts/pre-init-PACKAGE' and/or
;;   `cb-layouts/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cb-layouts-packages
  '(persp-mode
    eyebrowse)
  "The list of Lisp packages required by the cb-layouts layer.

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

(defun cb-layouts/post-init-persp-mode ()
  (spacemacs|define-custom-layout "Agenda+Mail"
    :binding "a"
    :body
    (progn
      (cb-org-goto-agenda)
      (let ((win (selected-window)))
        (select-window (split-window-sensibly))
        (mu4e)
        (select-window win))))

  (with-eval-after-load 'mu4e
    (defun cb-layouts/maybe-bury-mu4e-buffer ()
      (interactive)
      (unless (equal (spacemacs//current-layout-name) "Agenda+Mail")
        (bury-buffer)))
    (define-key mu4e-main-mode-map (kbd "q") #'cb-layouts/maybe-bury-mu4e-buffer))

  (with-eval-after-load 'org-agenda
    (defun cb-layouts/maybe-bury-agenda-buffer ()
      (interactive)
      (unless (equal (spacemacs//current-layout-name) "Agenda+Mail")
        (bury-buffer)))
    (define-key org-agenda-mode-map (kbd "q") #'cb-layouts/maybe-bury-agenda-buffer))

  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (push (lambda (buf)
            (with-current-buffer buf
              (derived-mode-p 'org-agenda-mode
                              'mu4e-view-mode
                              'mu4e-compose-mode
                              'mu4e-headers-mode
                              'mu4e-main-mode)))
          persp-filter-save-buffers-functions)))

(defun cb-layouts/post-init-eyebrowse ()
  (bind-key (kbd "<f5>") 'eyebrowse-switch-to-window-config-1)
  (bind-key (kbd "<f6>") 'eyebrowse-switch-to-window-config-2)
  (bind-key (kbd "<f7>") 'eyebrowse-switch-to-window-config-3)
  (bind-key (kbd "<f8>") 'eyebrowse-switch-to-window-config-4))

;;; packages.el ends here
