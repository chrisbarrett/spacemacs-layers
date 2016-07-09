;;; packages.el --- cb-js Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'smartparens nil t)
  (require 'use-package nil t))

(defconst cb-js-packages
  '(js2-mode
    web-mode
    js
    smartparens
    flycheck
    emmet-mode
    smart-ops
    (cb-flow-checker :location local)))

(defun cb-js/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.json\\'" . js2-mode))
    :init
    (progn
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
      (defalias 'json-mode 'js2-mode))
    :config
    (progn
      (setq js2-basic-offset 2)
      ;; Use flycheck for checking.
      (setq js2-mode-show-parse-errors nil)
      (setq js2-mode-show-strict-warnings nil))))

(defun cb-js/post-init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))

(defun cb-js/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (sp-with-modes '(js-mode js2-mode web-mode)
      (sp-local-pair "<" ">" :actions '(:rem insert))
      (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding)))))

(defun cb-js/post-init-smart-ops ()
  (use-package smart-ops
    :defer t
    :config
    (progn
      (add-hook 'web-mode-hook
                (lambda ()
                  (smart-ops-mode +1)))

      (let* ((inside-tags?
              (lambda (&rest _)
                (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
                  (equal op "<"))))

             (inside-squares?
              (lambda (&rest _)
                (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
                  (equal op "["))))

             (ops
              (list
               ;; Place point between empty angles and insert close marker.
               (smart-op "<>"
                         :pad-before nil :pad-after nil
                         :action (lambda (&rest _)
                                   (when (smart-ops-after-match? (rx "<" (* space) ">"))
                                     (search-backward ">")
                                     (delete-horizontal-space)
                                     (save-excursion
                                       (insert " /")))))

               (smart-ops "<" ">" :pad-unless inside-tags?)
               (smart-ops "=" "/" :pad-unless (-orfn inside-squares? inside-tags?))
               (smart-ops ";" ":" "," :pad-before nil)
               (smart-ops "=>" ">=")
               (smart-op "!" :bypass? t)
               (smart-ops-default-ops))))

        (apply' define-smart-ops-for-mode 'js-mode ops)
        (apply' define-smart-ops-for-mode 'js2-mode ops)
        (apply' define-smart-ops-for-mode 'web-mode ops)))))

(defun cb-js/post-init-web-mode ()
  (use-package web-mode
    :mode ("\\.jsx?\\'" . web-mode)
    :defer t
    :config
    (progn
      (remove-hook 'web-mode-hook #'spacemacs/toggle-smartparens-off)

      ;; Always treat .js files as JSX.
      (add-to-list 'web-mode-content-types '(("jsx" . "\\.js[x]?\\'")))

      ;; Use 2 spaces for indentation

      (defun cb-js/set-local-vars ()
        (setq web-mode-markup-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-code-indent-offset 2))

      (add-hook 'web-mode-hook #'cb-js/set-local-vars))))

(defun cb-js/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (progn
      (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
      (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
      (flycheck-add-mode 'javascript-eslint 'web-mode))))

(defun cb-js/init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :config
    (progn
      ;; Expand emmet snippets correctly when completing from inside a props
      ;; declaration, such that
      ;;
      ;;     foo[bar=baz|]
      ;;
      ;; expands correctly.

      (defun cb-js/forward-char (&rest _)
        (-when-let ((&plist :op op :end end) (sp-get-enclosing-sexp))
          (when (equal op "[" )
            (goto-char end))))

      (advice-add 'emmet-expand-yas :before #'cb-js/forward-char))))

(defun cb-js/init-cb-flow-checker ()
  (use-package cb-flow-checker
    :defer t
    :after flycheck
    :config
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))

;;; packages.el ends here
