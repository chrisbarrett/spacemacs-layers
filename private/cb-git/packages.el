(defvar cb-git-packages
  '(
    ;; package cb-gits go here
    magit
    git-gutter
    git-auto-commit-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-git-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-git/init-<package-cb-git>
;;
;; (defun cb-git/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
