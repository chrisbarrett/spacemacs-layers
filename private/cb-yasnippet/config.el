(require 'f)
(require 'yasnippet)

(custom-set-variables
 '(yas-snippet-dirs (list (f-join spacemacs-private-directory "cb-yasnippet" "snippets")))
 '(yas-prompt-functions '(yas-ido-prompt))
 '(yas-wrap-around-region t)
 '(yas-verbosity 0))

(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hook 'yas-minor-mode)
(yas-global-mode +1)

;;; Advise editing commands.
;;;
;;; Pressing SPC in an unmodified field will clear it and switch to the next.
;;;
;;; Pressing S-TAB to go to last field will place point at the end of the field.

(defadvice yas-next-field (before clear-blank-field activate)
  (yas/clear-blank-field))

(defadvice yas-prev-field (before clear-blank-field activate)
  (yas/clear-blank-field))

(defadvice yas-next-field (after goto-field-end activate)
  (yas/maybe-goto-field-end))

(defadvice yas-prev-field (after goto-field-end activate)
  (yas/maybe-goto-field-end))
