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

;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst cb-layouts-packages
  '(persp-mode
    eyebrowse))

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

  (use-package mu4e
    :init
    (defun cb-layouts/maybe-bury-mu4e-buffer ()
      (interactive)
      (unless (equal (spacemacs//current-layout-name) "Agenda+Mail")
        (bury-buffer)))
    :bind
    (:map mu4e-main-mode-map ("q" . cb-layouts/maybe-bury-mu4e-buffer)))

  (use-package org-agenda
    :init
    (defun cb-layouts/maybe-bury-agenda-buffer ()
      (interactive)
      (unless (equal (spacemacs//current-layout-name) "Agenda+Mail")
        (bury-buffer)))
    :bind
    (:map org-agenda-mode-map ("q" . cb-layouts/maybe-bury-agenda-buffer)))

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
  (bind-key (kbd "<f5>") #'eyebrowse-switch-to-window-config-1)
  (bind-key (kbd "<f6>") #'eyebrowse-switch-to-window-config-2)
  (bind-key (kbd "<f7>") #'eyebrowse-switch-to-window-config-3)
  (bind-key (kbd "<f8>") #'eyebrowse-switch-to-window-config-4))

;;; packages.el ends here
