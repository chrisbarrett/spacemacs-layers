;;; packages.el --- cb-yasnippet Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 's nil t)
  (require 'dash nil t)
  (require 'use-package nil t))

(defconst cb-yasnippet-packages
  '(yasnippet))

(defun cb-yasnippet/post-init-yasnippet ()
  (yas-global-mode +1)

  (cb-yas/register-snippets-dir (f-join user-layers-directory "cb-yasnippet/snippets"))

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
    (evil-define-key 'insert yas-keymap (kbd "SPC") 'yas/space))

  (spacemacs/declare-prefix "y" "yasnippet")
  (evil-leader/set-key "yf" 'yas-visit-snippet-file)
  (evil-leader/set-key "yn" 'yas-new-snippet)
  (evil-leader/set-key "yy" 'yas-insert-snippet)
  (evil-leader/set-key "yr" 'cb-yas/reload-all)

  ;; Advise editing commands.
  ;;
  ;; Pressing SPC in an unmodified field will clear it and switch to the next.
  ;;
  ;; Pressing S-TAB to go to last field will place point at the end of the field.

  (defadvice yas-next-field (before clear-blank-field activate)
    (yas/clear-blank-field))

  (defadvice yas-prev-field (before clear-blank-field activate)
    (yas/clear-blank-field))

  (defadvice yas-next-field (after goto-field-end activate)
    (yas/maybe-goto-field-end)
    (evil-insert-state))

  (defadvice yas-prev-field (after goto-field-end activate)
    (yas/maybe-goto-field-end)
    (evil-insert-state)))
