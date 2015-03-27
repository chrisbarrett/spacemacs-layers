(defvar cb-cosmetic-packages
  '(
    paren-face
    whitespace
    auto-highlight-symbol
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-cosmetic-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t))

(defun cb-cosmetic/init-paren-face ()
  (use-package paren-face
    :commands global-paren-face-mode
    :init (global-paren-face-mode)
    :config
    (progn
      (custom-set-faces
       '(parenthesis
         ((((background light)) :foreground "grey80")
          (((background dark))  :foreground "#505070"))))

      (add-to-list 'paren-face-modes 'haskell-mode)
      (add-to-list 'paren-face-modes 'haskell-interactive-mode)
      (add-to-list 'paren-face-modes 'idris-mode)
      (add-to-list 'paren-face-modes 'agda2-mode)
      (add-to-list 'paren-face-modes 'coq-mode))))

(defun cb-cosmetic/init-whitespace ()
  (use-package whitespace
    :diminish whitespace-mode
    :config
    (progn
      (setq whitespace-style '(face lines-tail))

      (defun core/maybe-enable-whitespace-mode ()
        (unless (or (derived-mode-p 'haskell-mode 'org-mode)
                    (and (boundp 'org-src-mode) org-src-mode))
          (whitespace-mode +1)
          (setq whitespace-line-column fill-column)))

      (add-hook 'prog-mode-hook 'core/maybe-enable-whitespace-mode)

      ;; HACK: override Spacemacs setting
      (add-hook 'prog-mode-hook
                (lambda ()
                  (setq show-trailing-whitespace nil))
                t)

      (defadvice whitespace-turn-on (around ignore-errors activate)
        "Ignore void-function errors when starting whitespace mode."
        (condition-case _
            ad-do-it
          (void-function))))))


(defun cb-cosmetic/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :config
    (progn

      ;;; Compatability with other packages

      (add-hook 'yas-after-exit-snippet-hook (lambda () (auto-highlight-symbol-mode +1)))
      (add-hook 'yas-before-expand-snippet-hook (lambda () (auto-highlight-symbol-mode -1)))

      (add-hook 'iedit-mode-end-hook (lambda () (auto-highlight-symbol-mode +1)))
      (add-hook 'iedit-aborting-hook (lambda () (auto-highlight-symbol-mode +1)))
      (add-hook 'iedit-mode-hook (lambda () (auto-highlight-symbol-mode -1))))))
