;;; packages.el --- cb-rust Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'skeletor nil t)
  (require 'smartparens nil t)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-rust-packages
  '(smart-ops
    company
    racer
    rust-mode
    flycheck
    skeletor
    smartparens
    aggressive-indent))

(defun cb-rust/post-init-flycheck ()
  (use-package flycheck
    :after rust-mode
    :config
    (setq flycheck-rust-cargo-executable "~/.cargo/bin/cargo")))

(defun cb-rust/post-init-company ()
  (with-eval-after-load 'rust-mode
    (use-package company
      :bind
      (:map rust-mode-map ("TAB" . company-indent-or-complete-common))
      :evil-bind
      (:map rust-mode-map
            :state normal ("TAB" . company-indent-or-complete-common)
            :state insert ("TAB" . company-indent-or-complete-common))
      :config
      (add-hook 'rust-mode-hook (lambda ()
                                  (setq company-minimum-prefix-length 2))))))

(defun cb-rust/post-init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (defun cb-rust/join-line ()
      "Join lines, deleting intermediate spaces for chained function calls."
      (interactive)
      (call-interactively #'evil-join)
      (when (thing-at-point-looking-at (rx (not space) (* space) "."))
        (delete-horizontal-space)))

    :evil-bind
    (:map rust-mode-map :state normal ("J" . cb-rust/join-line))))

(defun cb-rust/post-init-racer ()
  (use-package racer
    :after rust-mode
    :evil-bind
    (:map racer-mode-map :state normal
          ("M-." . racer-find-definition))))

(defun cb-rust/post-init-smart-ops ()
  (add-hook 'rust-mode-hook #'smart-ops-mode)
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops ">" ">>" ">>>" :pad-unless
               (lambda (&rest _)
                 (smart-ops-after-match? (rx "<"))))

    (smart-ops ".." "|" "::*" "::*;" "::<" :pad-before nil :pad-after nil)
    (smart-ops ";" "~" ":" "," :pad-before nil)
    (smart-ops ">," ">>," ">>>," :pad-before nil :pad-after t)
    (smart-ops "&" "!" :bypass? t)

    ;; Trait bounds

    (smart-ops ":>" ":>," ":>>," :pad-before nil :pad-after nil
               :action
               (lambda (&rest _)
                 (skip-chars-backward ">,")
                 (just-one-space)))

    (smart-ops "+>" "+>," "+>>," :pad-before t :pad-after nil
               :action
               (lambda (&rest _)
                 (skip-chars-backward ">,")
                 (just-one-space)))

    (smart-ops ":|" ",|"
               :pad-before nil
               :pad-after t
               :action
               (lambda (&rest _)
                 (skip-chars-backward "|")
                 (just-one-space)))

    ;; Record wildcards

    (smart-ops ",.."
               :pad-before nil
               :pad-after t
               :action
               (lambda (&rest _)
                 (save-excursion
                   (skip-chars-backward ".")
                   (just-one-space))))

    ;; References

    (smart-ops ":&" ",&"
               :pad-before nil
               :pad-after nil
               :action
               (lambda (&rest _)
                 (save-excursion
                   (skip-chars-backward "&")
                   (just-one-space))))

    (smart-ops "->&"
               :pad-before t
               :pad-after nil
               :action
               (lambda (&rest _)
                 (save-excursion
                   (skip-chars-backward "&")
                   (just-one-space))))

    ;; Scope accessors trigger company completion.

    (smart-ops "." "::" ">::" ">>::"
               :pad-before nil :pad-after nil
               :action #'company-manual-begin)

    (smart-op "::<" :pad-before nil :pad-after nil)

    (smart-op "->"
              :action (lambda (&rest _)
                        (when (s-matches? (rx (* space) "{") (buffer-substring (1- (point)) (line-end-position)))
                          (save-excursion
                            (save-restriction
                              (narrow-to-region (point) (line-end-position))
                              (just-one-space))))))

    ;; Position point inside template braces.
    (smart-ops "<>" "<>>" "<>>>" "<>>>>" "<>>>>>"
               :pad-before nil
               :pad-after-if
               (lambda (end)
                 (s-matches? (rx (* space) "{")
                             (buffer-substring end (line-end-position))))
               :action (lambda (&rest _)
                         (skip-chars-backward " >")))
    (smart-ops "<," "<>," "<>>," "<>>>," "<>>>>," "<>>>>>,"
               "<;" "<>;" "<>>;" "<>>>;" "<>>>>;" "<>>>>>;"
               "<::" "<>::" "<>>::" "<>>>::" "<>>>>::" "<>>>>>::"
               :pad-before nil :pad-after t
               :action (lambda (&rest _)
                         (skip-chars-backward " >")))
    (smart-ops ",>," ",>>," ",>>>," ",>>>>," ",>>>>>,"
               ",>;" ",>>;" ",>>>;" ",>>>>;" ",>>>>>;"
               ",>::" ",>>::" ",>>>::" ",>>>>::" ",>>>>>::"
               :pad-before nil :pad-after t
               :action (lambda (&rest _)
                         (skip-chars-backward " >")
                         (just-one-space)))

    ;; Dereferencing

    (smart-ops "*"
               :pad-after-unless
               (lambda (end)
                 (save-excursion
                   (goto-char (1- end))
                   (smart-ops--line-empty-up-to-point?))))

    (smart-ops "=*" "|*" :pad-after nil
               :action
               (lambda (&rest _)
                 (save-excursion
                   (skip-chars-backward "*")
                   (just-one-space))))

    (smart-ops ",*" ">,*" :pad-before nil :pad-after nil
               :action
               (lambda (&rest _)
                 (save-excursion
                   (skip-chars-backward "* ")
                   (just-one-space))))

    ;; Assignments

    (smart-ops "=&" "=|"
               :pad-after nil
               :action
               (lambda (&rest _)
                 (save-excursion
                   (search-backward "=")
                   (forward-char 1)
                   (just-one-space))))

    (smart-ops "=||"
               :action
               (lambda (&rest _)
                 (save-excursion
                   (search-backward "=")
                   (forward-char 1)
                   (just-one-space))
                 (search-backward "|")))

    ;; Inserting this op means you're probably adding a type annotation. Pad
    ;; internally and move point inside.
    (let ((inserting-type? (smart-ops-before-match? (rx bos (* space) "="))))
      (smart-ops ":="
                 :action
                 (lambda (&rest _)
                   (when (funcall inserting-type? (point))
                     (just-one-space)
                     (skip-chars-backward " =")
                     (just-one-space)
                     (save-excursion
                       (insert " ")
                       (skip-chars-backward " :")
                       (delete-horizontal-space))))))

    (smart-ops-default-ops)))

(defun cb-rust/post-init-skeletor ()

  (defun cb-rust/build-cargo-flags-interactively ()
    (pcase (completing-read "Project type: " '("binary" "library") nil t)
      ("binary" "--bin")
      ("library" "")))

  (use-package skeletor
    :config
    (skeletor-define-constructor "Rust"
      :requires-executables '(("cargo" . "https://www.rust-lang.org/downloads.html"))
      :no-git? t
      :initialise
      (lambda (spec)
        (let-alist spec
          (skeletor-shell-command
           (format "cargo new %s %s" (shell-quote-argument .project-name) (cb-rust/build-cargo-flags-interactively))
           .project-dir)))
      :after-creation
      (lambda (dir)
        (skeletor-shell-command "git commit --allow-empty -m 'Initial commit'" dir)
        (skeletor-shell-command "git add -A && git commit -m 'Add initial files'" dir)))))

(defun cb-rust/post-init-smartparens ()
  (use-package smartparens
    :config
    (progn
      ;; HACK: There seems to be a race condition setting up SP pairs in
      ;; rust-mode :/

      (defun cb-rust--configure-sp ()
        (sp-with-modes 'rust-mode
          (sp-local-pair "{" "}" :post-handlers '(:add sp-internal-and-external-padding))
          (sp-local-pair "'" "'" :actions '(:rem insert))
          (sp-local-pair "<" ">" :actions '(:rem insert))))

      (add-hook 'rust-mode-hook #'cb-rust--configure-sp))))

(defun cb-rust/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (add-to-list 'aggressive-indent-excluded-modes 'rust-mode)))

(provide 'packages)

;;; packages.el ends here
