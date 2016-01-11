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

  (with-eval-after-load 'yasnippet
    (yas/reload-all)
    (bind-key "<backspace>" 'yas/backspace yas-keymap)
    (evil-define-key 'insert yas-keymap (kbd "SPC") #'yas/space))

  (spacemacs/declare-prefix "Y" "yasnippet")
  (spacemacs/set-leader-keys "Yf" 'yas-visit-snippet-file)
  (spacemacs/set-leader-keys "Yn" 'yas-new-snippet)
  (spacemacs/set-leader-keys "Yy" 'yas-insert-snippet)
  (spacemacs/set-leader-keys "Yr" 'cb-yas/reload-all)

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
