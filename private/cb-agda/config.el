(if (executable-find "agda-mode")
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
  (warn "`agda-mode' shell command not on path.  `agda-mode' not available."))


(after 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'agda2-mode))

(add-hook 'agda2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'agda2-load nil t)
            (add-hook 'before-save-hook 'agda/rewrite-symbols-in-buffer nil t)))

(add-to-list 'face-remapping-alist '(agda2-highlight-error-face . flycheck-error))
(add-to-list 'face-remapping-alist '(agda2-highlight-keyword-face . font-lock-keyword-face))
(add-to-list 'face-remapping-alist '(agda2-highlight-bound-variable-face . font-lock-variable-name-face))
(add-to-list 'face-remapping-alist '(agda2-highlight-module-face . font-lock-constant-face))
(add-to-list 'face-remapping-alist '(agda2-highlight-datatype-face . font-lock-type-face))
(add-to-list 'face-remapping-alist '(agda2-highlight-function-face . default))
(add-to-list 'face-remapping-alist '(agda2-highlight-primitive-type-face . font-lock-builtin-face))
(add-to-list 'face-remapping-alist '(agda2-highlight-symbol-face . default))

(custom-set-faces
 `(agda2-highlight-number-face
   ((t
     (:foreground ,solarized-hl-magenta))))

 `(agda2-highlight-inductive-constructor-face
   ((t
     (:foreground ,solarized-hl-violet :italic t))))

 `(agda2-highlight-coinductive-constructor-face
   ((t
     (:foreground ,solarized-hl-magenta :italic t)))))
