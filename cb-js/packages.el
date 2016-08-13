;;; packages.el --- cb-js Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'smartparens nil t)
  (require 'skeletor nil t)
  (require 'cb-use-package-extensions)
  (require 'use-package nil t))

(defconst cb-js-packages
  '(web-mode
    js
    smartparens
    flycheck
    emmet-mode
    smart-ops
    nodejs-repl
    skeletor
    (cb-flow-checker :location local)
    (cb-web-modes :location local)
    (js-yasnippet :location local)
    (cb-emmet-tolerant-expansion :location local)
    (cb-flow :location local)
    (cb-js-smart-ops :location local)))

(defun cb-js/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (progn
      (defun cb-js/format-after-paren (_id action context)
        "Insert a space after some keywords."
        (when (and (equal action 'insert)
                   (equal context 'code)
                   (thing-at-point-looking-at
                    (rx symbol-start (or "=" "for" "of" "in"
                                         "if" "else" "while"
                                         "return"
                                         "yield" "yield*"
                                         "function" "function*")
                        (* space) "(")))
          (save-excursion
            (search-backward "(")
            (just-one-space))))

      (sp-with-modes 'cb-web-js-mode
        (sp-local-pair "(" ")" :post-handlers '(:add cb-js/format-after-paren))
        (sp-local-pair "<" ">" :actions '(:rem insert)))

      (defun js/sp-braces-external-padding (id action ctx)
        (when (and (equal action 'insert)
                   (equal ctx 'code))
          (-when-let* ((end (point))
                       (beg (save-excursion
                              (search-backward (sp-get-pair id :open)
                                               (line-beginning-position) t))))
            (cond
             ((s-matches? (rx bol (* space) "<" (*? nonl) "=" (* space) eol)
                          (buffer-substring (line-beginning-position) beg))
              ;; Delete leading spaces
              (save-excursion
                (goto-char beg)
                (delete-horizontal-space))
              ;; Delete trailing spaces before end of tag marker.
              (save-excursion
                (forward-char)
                (skip-chars-forward " ")
                (when (equal (char-after) ?>)
                  (delete-horizontal-space))))
             (t
              (sp-external-padding id action ctx)))
            t)))

      (sp-with-modes '(cb-web-js-mode cb-web-json-mode)
        (sp-local-pair "{" "}" :post-handlers '(:add js/sp-braces-external-padding))))))

(defun cb-js/post-init-smart-ops ()
  (use-package smart-ops
    :defer t
    :config (add-hook 'web-mode-hook (lambda () (smart-ops-mode +1)))))

(defun cb-js/post-init-web-mode ()
  ;; HACK: Delete web-mode auto-mode config set by Spacemacs so that I can use
  ;; specialised derived modes instead.
  (setq auto-mode-alist
        (-remove (-lambda ((_ . mode))
                   (equal 'web-mode mode))
                 auto-mode-alist))

  (use-package web-mode
    :defer t
    :config
    (progn
      (remove-hook 'web-mode-hook #'spacemacs/toggle-smartparens-off)

      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-auto-pairing nil)

      ;; Always treat .js files as JSX.

      (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))

      ;; Use 2 spaces for indentation

      (defun cb-js/set-local-vars ()
        (setq-local web-mode-enable-auto-quoting nil)
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
      (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode))))

(defun cb-js/post-init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :config
    (progn
      (defun cb-js/set-jsx-classname-on ()
        (setq-local emmet-expand-jsx-className? t))

      (add-hook 'cb-web-js-mode-hook #'cb-js/set-jsx-classname-on))))

(defun cb-js/init-cb-flow-checker ()
  (use-package cb-flow-checker
    :defer t
    :after flycheck
    :config
    (progn
      (add-hook 'cb-web-js-mode-hook (lambda ()
                                       (flycheck-select-checker 'javascript-flow)))
      (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))))

(defun cb-js/init-nodejs-repl ()
  (use-package nodejs-repl
    :commands nodejs-repl
    :init
    (with-eval-after-load 'cb-web-modes
      (define-key cb-web-js-mode-map (kbd "C-c C-z") #'nodejs-repl))
    :config
    (progn
      (evil-set-initial-state 'nodejs-repl-mode 'insert)

      (defun cb-js/switch-to-src ()
        "Pop to the last JS source buffer."
        (interactive)
        (-if-let ((buf) (cb-buffers-filtera (derived-mode-p 'cb-web-js-mode)))
            (pop-to-buffer buf)
          (error "No JS buffers")))

      (define-key nodejs-repl-mode-map (kbd "C-c C-z") #'cb-js/switch-to-src))))

(defun cb-js/post-init-skeletor ()
  (use-package skeletor
    :config
    (skeletor-define-constructor "JavaScript (Node)"
      :requires-executables '(("npm" . "https://docs.npmjs.com/getting-started/installing-node"))
      :initialise
      (lambda (x)
        (-let [(&alist 'project-dir dir) x]
          (skeletor--log-info "Generating project...")
          (skeletor-shell-command "npm init --yes" dir))))))

(defun cb-js/init-cb-web-modes ()
  (use-package cb-web-modes
    :defer t
    :mode (("\\.json\\'" . cb-web-json-mode)
           ("\\.jsx?\\'" . cb-web-js-mode)
           ("\\.html\\'" . cb-web-html-mode))
    :config
    (with-eval-after-load 'flycheck
      (flycheck-add-mode 'json-jsonlint 'cb-web-json-mode))))

(defun cb-js/init-js-yasnippet ()
  (use-package js-yasnippet
    :after yasnippet))

(defun cb-js/init-cb-emmet-tolerant-expansion ()
  (use-package cb-emmet-tolerant-expansion
    :after emmet-mode
    :functions cb-emmet-tolerant-expansion-init
    :config (cb-emmet-tolerant-expansion-init)))

(defun cb-js/init-cb-flow ()
  (use-package cb-flow
    :after cb-web-modes
    :leader-bind
    (:map cb-web-js-mode-map
          ("if" . cb-flow-insert-flow-annotation))
    :bind (:map cb-web-js-mode-map
                ("C-c C-t" . cb-flow-type-at))))

(defun cb-js/init-cb-js-smart-ops ()
  (use-package cb-js-smart-ops
    :after smart-ops
    :config (cb-js-smart-ops-init)))

;;; packages.el ends here
