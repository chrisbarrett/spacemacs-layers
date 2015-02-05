(defvar cb-project-packages
  '(
    ;; package cb-projects go here
    projectile
    skeletor
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


(defun cb-project/init-projectile ()
  (use-package projectile
    :config
    (progn

      (defadvice projectile-cache-current-file (around ignore-errors activate)
        (ignore-errors ad-do-it))

      (setq projectile-cache-file (concat spacemacs-cache-directory "projectile.cache"))
      (setq projectile-ignored-projects '("/usr/local/"))
      (setq projectile-switch-project-action (lambda () (call-interactively 'magit-status)))
      (setq projectile-globally-ignored-directories
            '(".cask"
              ".cabal-sandbox"
              "dist"
              ".idea"
              "target"
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
              ".ensime_cache")))))

(defun cb-project/init-skeletor ()
  (use-package skeletor
    :commands (skeletor-create-project skeletor-create-project-at)
    :config
    (progn
      (setq skeletor-show-project-command 'magit-status)
      (setq skeletor-scala-use-ensime t)
      (setq skeletor-user-directory (concat spacemacs-private-directory "cb-project/project-skeletons")))))
