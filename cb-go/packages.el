;;; packages.el --- cb-go layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Barrett <admiral@walrus.cool>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cb-go-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cb-go/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cb-go/pre-init-PACKAGE' and/or
;;   `cb-go/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(eval-when-compile
  (require 'cb-use-package-extensions nil t))

(defconst cb-go-packages
  '(go-mode
    company-go
    go-eldoc
    go-rename
    (go-oracle :location site)
    flycheck
    flycheck-gometalinter
    autoinsert
    smartparens
    (cb-go-smart-ops :location local)
    (cb-go-run :location local)))

(defun cb-go/init-go-mode ()
  (use-package go-mode
    :defer t
    :leader-bind
    (:mode go-mode
           ("hh" . godoc-at-point)
           ("ig" . go-goto-imports)
           ("ia" . go-import-add)
           ("ir" . go-remove-unused-imports)
           ("eb" . go-play-buffer)
           ("er" . go-play-region)
           ("ed" . go-download-play)
           ("ga" . ff-find-other-file)
           ("gc" . go-coverage))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "me" "playground")
      (spacemacs/declare-prefix-for-mode 'go-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'go-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'go-mode "mi" "imports")

      (defun cb-go--set-local-vars ()
        (setq-local tab-width 4)
        (setq-local evil-shift-width 4))

      (add-hook 'go-mode-hook #'cb-go--set-local-vars)
      (add-hook 'before-save-hook #'gofmt-before-save))))

(defun cb-go/init-company-go ()
  (use-package company-go
    :after go-mode
    :config
    (progn
      (require 'company)
      (setq company-go-show-annotation t)
      (push 'company-go company-backends-go-mode))))

(defun cb-go/post-init-flycheck ()
  (use-package flycheck
    :config
    (add-to-list 'flycheck-global-modes 'go-mode)))

(defun cb-go/init-go-eldoc ()
  (use-package go-eldoc
    :after go-mode
    :config
    (add-hook 'go-mode-hook #'go-eldoc-setup)))

(defun cb-go/init-flycheck-gometalinter ()
  (use-package flycheck-gometalinter
    :after go-mode
    :init
    (progn
      (defun cb-go--configure-metalinter ()
        "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
        (setq flycheck-disabled-checkers '(go-gofmt
                                           go-golint
                                           go-vet
                                           go-build
                                           go-test
                                           go-errcheck))
        (flycheck-gometalinter-setup))

      (add-hook 'go-mode-hook #'cb-go--configure-metalinter t))))

(defun cb-go/init-go-oracle ()
  (setq go-path (or (getenv "GOPATH") "~/golang"))
  (setq oracle-path (f-join go-path "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
  (use-package go-oracle
    :after go-mode
    :load-path oracle-path
    :leader-bind
    (:mode go-mode
           ("ro" . go-oracle-set-scope)
           ("r<" . go-oracle-callers)
           ("r>" . go-oracle-callees)
           ("rc" . go-oracle-peers)
           ("rd" . go-oracle-definition)
           ("rf" . go-oracle-freevars)
           ("rg" . go-oracle-callgraph)
           ("ri" . go-oracle-implements)
           ("rp" . go-oracle-pointsto)
           ("rr" . go-oracle-referrers)
           ("rs" . go-oracle-callstack)
           ("rt" . go-oracle-describe))
    :config
    (spacemacs/declare-prefix-for-mode 'go-mode "mr" "rename")))

(defun cb-go/init-go-rename()
  (use-package go-rename
    :after go-mode
    :leader-bind
    (:mode go-mode ("rn" . go-rename))))

(defun cb-go/post-init-autoinsert ()
  (use-package autoinsert
    :config
    (add-to-list 'auto-insert-alist
                 '(("\\.go\\'" . "Go Src File")
                   nil
                   "package " (s-lower-camel-case (file-name-base)) "\n"
                   "\n"
                   _
                   "\n"))))

(defun cb-go/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-with-modes 'go-mode
      (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding)))))

(defun cb-go/init-cb-go-smart-ops ()
  (use-package cb-go-smart-ops
    :after go-mode
    :config (cb-go-smart-ops-init)))

(defun cb-go/init-cb-go-run ()
  (use-package cb-go-run
    :after go-mode
    :leader-bind
    (:mode go-mode
           ("tt" . cb-go-run-test-current-function)
           ("ts" . cb-go-run-test-current-suite)
           ("tp" . cb-go-run-package-tests)
           ("tP" . cb-go-run-package-tests-nested)
           ("xx" . cb-go-run-main))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'go-mode "mx" "execute"))))

;;; packages.el ends here
