(defvar cb-csharp-packages
  '(
    ;; package cb-csharps go here
    omnisharp
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-csharp-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-csharp/init-<package-cb-csharp>
;;
;; (defun cb-csharp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package


(defun cb-csharp/init-omnisharp ()
  (use-package omnisharp
    :commands omnisharp-mode
    :config
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))
