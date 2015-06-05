;;; packages.el --- cb-project Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-project-packages
  '(
    projectile
    skeletor
    helm-projectile
    neotree
    helm-ag
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-project-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t)
  (require 's nil t))

(defun cb-project/init-projectile ()
  (use-package projectile
    :bind
    (("s-f" . projectile-find-file)
     ("s-F" . project/find-file-in-scope)
     ("s-d" . projectile-find-dir)
     ("s-l" . projectile-switch-project))
    :init
    (evil-leader/set-key "pa" 'projectile-ag)
    :config
    (progn
      ;;; Vars

      (setq projectile-ignored-projects '("/usr/local/"))
      (setq projectile-switch-project-action (lambda () (call-interactively 'magit-status)))
      (setq projectile-globally-ignored-directories
            '(".cask"
              ".cabal-sandbox"
              "dist"
              ".idea"

              ".eunit"
              ".git"
              ".hg"
              ".fslckout"
              ".bzr"
              "_darcs"
              ".tox"
              ".svn"
              "elpa"
              "snippets"
              "build"

              ;; Scala
              ".sbtserver"
              "target"
              "project/target"
              "project/project"
              ".ensime_cache"
              ))

      (setq ag-ignore-list (-map 'regexp-quote projectile-globally-ignored-directories))

      (defadvice projectile-invalidate-cache (before recentf-cleanup activate)
        (recentf-cleanup))

      (add-hook 'after-init-hook
                (lambda ()
                  (setq projectile-completion-system 'helm)
                  (projectile-cleanup-known-projects)))

      ;; Advice

      (defadvice projectile-cache-current-file (around ignore-errors activate)
        (ignore-errors ad-do-it))

      (defadvice projectile-replace (around save-window-excursion activate)
        (save-window-excursion ad-do-it)))))

(defun cb-project/init-helm-projectile ()
  (use-package helm-projectile
    :bind
    (("s-t" . helm-projectile))))

(defun cb-project/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
    (setq helm-ag-insert-at-point 'symbol)))

(defun cb-project/init-skeletor ()
  (use-package skeletor
    :commands (skeletor-create-project skeletor-create-project-at)
    :config
    (progn
      (setq skeletor-show-project-command 'magit-status)
      (setq skeletor-scala-use-ensime t)
      (setq skeletor-user-directory (concat spacemacs-private-directory "cb-project/project-skeletons")))))

(use-package neotree
  :defer t
  :config
  (progn
    (core/remap-face 'neo-dir-link-face 'default)
    (set-face-foreground neo-file-link-face solarized-hl-orange)
    (set-face-foreground neo-root-dir-face solarized-hl-blue)))
