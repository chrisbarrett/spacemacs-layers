(defvar cb-spelling-packages
  '(
    ;; package cb-spellings go here
    ispell
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-spelling-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-spelling/init-<package-cb-spelling>
;;
;; (defun cb-spelling/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-spelling/init-ispell ()
  (use-package ispell
    :init
    (progn
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      (add-hook 'nxml-mode-hook 'flyspell-prog-mode)
      (add-hook 'sgml-mode-hook 'flyspell-prog-mode))
    :config
    (custom-set-variables
     '(ispell-program-name "aspell")
     '(ispell-dictionary "en_GB")
     '(ispell-silently-savep t))))
