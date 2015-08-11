;;; packages.el --- cb-cpp Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-cpp-packages
  '(irony
    company-irony
    company-irony-c-headers
    irony-eldoc
    flycheck-irony
    google-c-style
    ggtags
    helm-gtags
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-cpp-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-cpp/init-irony ()
  (use-package irony
    :commands (irony-mode irony-install-server)
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    :config
    (progn
      (setq irony-user-dir (f-slash (f-join user-home-directory "bin" "irony")))
      (setq irony-server-install-prefix irony-user-dir)
      (setq irony-additional-clang-options '("-std=c++14"))

      (defun cb-cpp/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

      (add-hook 'irony-mode-hook 'cb-cpp/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))

(defun cb-cpp/init-company-irony ()
  (use-package company-irony
    :config
    (progn
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'company-irony))

      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))

(defun cb-cpp/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun cb-cpp/init-flycheck-irony ()
  (use-package flycheck-irony
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))

(defun cb-cpp/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends '(company-irony-c-headers)))))

(defun cb-cpp/init-google-c-style ()
  (use-package google-c-style
    :commands google-set-c-style
    :init
    (progn
      (setq-default clang-format-style "Google")
      (add-hook 'c-mode-common-hook 'google-set-c-style))))

(defun cb-cpp/init-ggtags ()
  (use-package ggtags
    :commands ggtags-mode
    :init
    (add-hook 'c++-mode-hook 'ggtags-mode)
    :config
    (set-face-underline 'ggtags-highlight nil)))

(defun cb-cpp/init-helm-gtags ()
  (use-package helm-gtags
    :init (add-hook 'c++-mode-hook 'helm-gtags-mode)))
