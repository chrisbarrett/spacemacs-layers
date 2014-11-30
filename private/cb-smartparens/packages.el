(defvar cb-smartparens-packages
  '(
    ;; package cb-smartparenss go here
    smartparens
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-smartparens-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-smartparens/init-<package-cb-smartparens>
;;
;; (defun cb-smartparens/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-smartparens/init-smartparens ()
  (use-package smartparens
    :commands (smartparens-global-mode show-smartparens-global-mode)
    :init nil
    (progn
      (smartparens-global-mode)
      (show-smartparens-global-mode +1))
    :config
    (progn
      (custom-set-variables
       '(sp-autoinsert-if-followed-by-word t)
       '(sp-navigate-close-if-unbalanced t)
       '(sp-message-width nil))

      (add-hook 'prog-mode-hook 'smartparens-strict-mode)
      (add-hook 'markdown-mode-hook 'smartparens-strict-mode)
      (add-hook 'ielm-mode-hook 'smartparens-strict-mode)

      (add-hook 'minibuffer-setup-hook 'sp/maybe-enable-smartparens t)
      (add-hook 'minibuffer-inactive-mode-hook 'sp/maybe-enable-smartparens t)

      (add-hook 'smartparens-mode-hook 'sp/hacky-set-sp-bindings t)
      (add-hook 'smartparens-strict-mode-hook 'sp/hacky-set-sp-bindings t))))
