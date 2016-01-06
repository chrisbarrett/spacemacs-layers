;;; packages.el --- cb-workspaces Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-workspaces-packages '(eyebrowse))

(defun cb-workspaces/post-init-eyebrowse ()
  (bind-key (kbd "<f5>") 'eyebrowse-switch-to-window-config-1)
  (bind-key (kbd "<f6>") 'eyebrowse-switch-to-window-config-2)
  (bind-key (kbd "<f7>") 'eyebrowse-switch-to-window-config-3)
  (bind-key (kbd "<f8>") 'eyebrowse-switch-to-window-config-4))
