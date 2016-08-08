;;; packages.el --- cb-yasnippet Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash)
  (require 'f)
  (require 's)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(autoload 'evil-insert-state "evil-states")

(defconst cb-yasnippet-packages
  '(yasnippet
    smartparens))

(defvar cb-yasnippet/main-snippets-dir
  (f-join user-layers-directory "cb-yasnippet/snippets"))

(defun cb-yasnippet/post-init-yasnippet ()
  (use-package yasnippet

    :init
    (spacemacs/declare-prefix "y" "yasnippet")
    :leader-bind
    (("yf" . yas-visit-snippet-file)
     ("ye" . yas-expand)
     ("yn" . yas-new-snippet)
     ("yy" . yas-insert-snippet)
     ("yr" . cb-yas/reload-all))

    :bind
    (:map prog-mode-map ("TAB" . yas-expand)
          :map yas-keymap ("<backspace>" . yas/backspace))

    :evil-bind
    (:state insert
            :map yas-minor-mode-map ("TAB" . yas-expand)
            :map yas-keymap ("SPC" . yas/space))

    :config
    (progn
      (setq yas-snippet-dirs cb-yasnippet/yas-dirs)
      (setq yas-prompt-functions '(yas-ido-prompt))
      (setq yas-wrap-around-region t)
      (setq yas-verbosity 0)
      (setq yas-triggers-in-field nil)


      (cb-yas/register-snippets-dir cb-yasnippet/main-snippets-dir)

      (cb-remap-face 'yas-field-highlight-face 'cb-faces-bg-hl-template)

      (add-hook 'yas-minor-mode-hook #'cb-yas/sync-with-yasnippet)
      (add-hook 'snippet-mode-hook (lambda () (setq-local require-final-newline nil)))

      ;; Advise editing commands.
      ;;
      ;; Pressing SPC in an unmodified field will clear it and switch to the next.
      ;;
      ;; Pressing S-TAB to go to last field will place point at the end of the field.

      (defun cb-yasnippet/goto-field-end (&rest _)
        (yas/maybe-goto-field-end)
        (when (and (boundp 'evil-mode) evil-mode)
          (evil-insert-state)))

      (advice-add 'yas-next-field :before #'yas/clear-blank-field)
      (advice-add 'yas-prev-field :before #'yas/clear-blank-field)
      (advice-add 'yas-next-field :after #'cb-yasnippet/goto-field-end)
      (advice-add 'yas-prev-field :after #'cb-yasnippet/goto-field-end)

      ;; Ensure yasnippet expansion preserves current indentation. This can be a
      ;; problem in modes with significant whitespace, where the indentation
      ;; command unconditionally indents one step.

      (defun cb-yasnippet/preserve-indentation (f &rest args)
        (let ((col
               (save-excursion
                 (back-to-indentation)
                 (current-column))))
          (apply f args)
          (save-excursion
            (atomic-change-group
              (goto-char (line-beginning-position))
              (delete-horizontal-space)
              (indent-to col)))))

      (advice-add 'yas--expand-or-prompt-for-template :around #'cb-yasnippet/preserve-indentation)

      (spacemacs|diminish yas-minor-mode " ⓨ" " y")
      (yas-reload-all)
      (yas-global-mode +1))))

(defun cb-yasnippet/post-init-smartparens ()
  (use-package smartparens
    :config
    (progn
      ;; HACK: Work around Spacemacs issue with smartparens strict mode.

      (defun cb-yasnippet/disable-smartparens-strict-mode ()
        (smartparens-strict-mode -1))

      (defun cb-yasnippet/restore-smartparens-strict-mode ()
        (smartparens-strict-mode +1))

      (add-hook 'yas-before-expand-snippet-hook #'cb-yasnippet/disable-smartparens-strict-mode)
      (add-hook 'yas-after-exit-snippet-hook #'cb-yasnippet/restore-smartparens-strict-mode))))

;;; packages.el ends here
