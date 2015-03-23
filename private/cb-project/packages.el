(defvar cb-project-packages
  '(
    ;; package cb-projects go here
    projectile
    skeletor
    helm-projectile
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-project-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-project/init-<package-cb-project>
;;
;; (defun cb-project/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

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

(defun cb-project/init-skeletor ()
  (use-package skeletor
    :commands (skeletor-create-project skeletor-create-project-at)
    :config
    (progn
      (setq skeletor-show-project-command 'magit-status)
      (setq skeletor-scala-use-ensime t)
      (setq skeletor-user-directory (concat spacemacs-private-directory "cb-project/project-skeletons")))))
