;;; packages.el --- cb-js Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'smartparens nil t)
  (require 'skeletor nil t)
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
    (cb-emmet-tolerant-expansion :location local)))

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
    :config
    (progn
      (add-hook 'web-mode-hook
                (lambda ()
                  (smart-ops-mode +1)))

      (defun cb-js/inside-angles? (&rest _)
        ;; HACK: smartparens doesn't always detect angles correctly, so just
        ;; look for the start of a tag on the current line.
        (or (s-matches? (rx bol (* space) "<")
                        (buffer-substring (line-beginning-position)
                                          (line-end-position)))
            (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
              (equal op "<"))))

      (defun cb-js/inside-squares? (&rest _)
        (-when-let ((&plist :op op) (sp-get-enclosing-sexp))
          (equal op "[")))

      (defun cb-js/move-between-angles-insert-slash (&rest _)
        (when (smart-ops-after-match? (rx "<" (* space) ">"))
          (search-backward ">")
          (delete-horizontal-space)
          (save-excursion
            (insert " /"))))

      (define-smart-ops-for-mode 'cb-web-html-mode
        (smart-op "<>"
                  :pad-before nil :pad-after nil
                  :action #'cb-js/move-between-angles-insert-slash))

      (define-smart-ops-for-mode 'cb-web-json-mode
        (smart-ops ":" "," :pad-before nil))


      (let ((js-ops
             (list
              (smart-op "*" :pad-before-unless (smart-ops-after-match? (rx (or "yield" "function"))))
              (smart-op "<>"
                        :pad-before nil :pad-after nil
                        :action #'cb-js/move-between-angles-insert-slash)

              (smart-ops "<" ">" :pad-unless #'cb-js/inside-angles?)

              (smart-ops "=" "/" :pad-unless (-orfn #'cb-js/inside-squares?
                                                    #'cb-js/inside-angles?))

              ;; KLUDGE: This is the pair when inserting <tag attr=|/>
              (smart-ops "=/>" "=/>,"
                         :pad-before nil
                         :pad-after nil
                         :action
                         (lambda (&rest _)
                           (skip-chars-backward ">/ ")
                           (just-one-space)
                           (backward-char)))

              ;; KLUDGE: Could be an arrow, but could be inserting an attribute
              ;; inside a tag.
              (smart-ops "=>"
                         :pad-before-unless #'cb-js/inside-angles?
                         :action
                         (lambda (&rest _)
                           (when (cb-js/inside-angles?)
                             (skip-chars-backward "> ")
                             (just-one-space)
                             (backward-char))))

              (smart-ops ";" ":" "," :pad-before nil)
              (smart-ops "++" "--" "++;" "--;" :pad-before nil :pad-after nil)
              (smart-ops ">=")
              (smart-op "!" :bypass? t)
              (smart-ops-default-ops :pad-unless #'cb-js/inside-angles?))))

        (apply #'define-smart-ops-for-mode 'cb-web-js-mode js-ops)
        (apply #'define-smart-ops-for-mode 'nodejs-repl-mode js-ops)
        (add-hook 'nodejs-repl-mode-hook #'smart-ops-mode)))))

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
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))

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

;;; packages.el ends here
