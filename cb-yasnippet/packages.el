;;; packages.el --- cb-yasnippet Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'use-package)

(autoload 'evil-insert-state "evil-states")

(defconst cb-yasnippet-packages
  '(yasnippet))

(defvar cb-yasnippet/main-snippets-dir
  (f-join user-layers-directory "cb-yasnippet/snippets"))

(defun cb-yasnippet/post-init-yasnippet ()
  (yas-global-mode +1)

  (cb-yas/register-snippets-dir cb-yasnippet/main-snippets-dir)

  (add-hook 'yas-minor-mode-hook 'cb-yas/sync-with-yasnippet)

  ;; Set up snippet directories.
  (setq yas-snippet-dirs cb-yasnippet/yas-dirs)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (setq yas-wrap-around-region t)
  (setq yas-verbosity 0)
  (setq yas-triggers-in-field nil)

  (core/remap-face 'yas-field-highlight-face 'core/bg-hl-template)

  (add-hook 'snippet-mode-hook (lambda () (setq-local require-final-newline nil)))

  (bind-key (kbd "TAB") #'yas-expand prog-mode-map)

  (with-eval-after-load 'yasnippet
    (yas/reload-all)

    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)

    (bind-key "<backspace>" 'yas/backspace yas-keymap)
    (evil-define-key 'insert yas-keymap (kbd "SPC") #'yas/space))

  (spacemacs/declare-prefix "Y" "yasnippet")
  (spacemacs/set-leader-keys "Yf" 'yas-visit-snippet-file)
  (spacemacs/set-leader-keys "Yn" 'yas-new-snippet)
  (spacemacs/set-leader-keys "Yy" 'yas-insert-snippet)
  (spacemacs/set-leader-keys "Yr" 'cb-yas/reload-all)

  ;; HACK: Work around Spacemacs issue with smartparens strict mode.

  (defun cb-yasnippet/smartparens-strict-mode-active? ()
    (and smartparens-mode smartparens-global-strict-mode))

  (defconst cb-yasnippet/should-enable-smartparens-strict-mode (cb-yasnippet/smartparens-strict-mode-active?))

  (defun cb-yasnippet/disable-smartparens-strict-mode ()
    (let ((active? (cb-yasnippet/smartparens-strict-mode-active?)))
      (setq cb-yasnippet/should-enable-smartparens-strict-mode active?)
      (when active?
        (smartparens-strict-mode -1))))

  (defun cb-yasnippet/restore-smartparens-strict-mode ()
    (when cb-yasnippet/should-enable-smartparens-strict-mode
      (smartparens-strict-mode +1)))

  (add-hook 'yas-before-expand-snippet-hook #'cb-yasnippet/disable-smartparens-strict-mode)
  (add-hook 'yas-after-exit-snippet-hook #'cb-yasnippet/restore-smartparens-strict-mode)

  ;; Advise editing commands.
  ;;
  ;; Pressing SPC in an unmodified field will clear it and switch to the next.
  ;;
  ;; Pressing S-TAB to go to last field will place point at the end of the field.

  (defun cb-yasnippet/goto-field-end ()
    (yas/maybe-goto-field-end)
    (when (and (boundp 'evil-mode) evil-mode)
      (evil-insert-state)))

  (advice-add 'yas-next-field :before #'yas/clear-blank-field)
  (advice-add 'yas-prev-field :before #'yas/clear-blank-field)
  (advice-add 'yas-next-field :after #'cb-yasnippet/goto-field-end)
  (advice-add 'yas-prev-field :after #'cb-yasnippet/goto-field-end))

;;; packages.el ends here
