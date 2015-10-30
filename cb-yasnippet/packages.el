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

  ;; Set up snippet directories.
  (setq yas-snippet-dirs (list (f-join user-layers-directory "cb-yasnippet/snippets")))
  (setq yas-snippet-dirs (-uniq (--reject (or
                                           ;; Why is this symbol even in there? Jeez.
                                           (equal 'yas-installed-snippets-dir it)
                                           ;; Don't use snippets from packages.
                                           (s-matches? "/elpa/" it)
                                           ;; Ensure all paths exist or yasnippet will fail to load.
                                           (not (f-dir? it)))

                                          yas-snippet-dirs)))

  (setq yas-prompt-functions '(yas-ido-prompt))
  (setq yas-wrap-around-region t)
  (setq yas-verbosity 0)

  (core/remap-face 'yas-field-highlight-face 'core/bg-hl-template)

  (add-hook 'snippet-mode-hook (lambda () (setq-local require-final-newline nil)))

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
