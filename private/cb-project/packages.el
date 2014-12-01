(defvar cb-project-packages
  '(
    ;; package cb-projects go here
    ack-and-a-half
    projectile
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
    (setq projectile-cache-file (concat spacemacs-cache-directory "projectile.cache"))
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
           "snippets"
           "build"))))
