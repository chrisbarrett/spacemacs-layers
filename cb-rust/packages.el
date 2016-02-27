;;; packages.el --- cb-rust Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'skeletor nil t)
  (require 'smartparens nil t))

(defconst cb-rust-packages
  '(smart-ops
    skeletor
    smartparens))

(defun cb-rust/post-init-smart-ops ()
  (add-hook 'rust-mode-hook #'smart-ops-mode)
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops ";" "," "~" "&" ":" :pad-before nil)

    (smart-ops "." "::"
               :pad-before nil :pad-after nil
               :action #'company-manual-begin)

    (smart-op "->"
              :action (lambda (&rest _)
                        (when (s-matches? (rx (* space) "{") (buffer-substring (1- (point)) (line-end-position)))
                          (save-excursion
                            (save-restriction
                              (narrow-to-region (point) (line-end-position))
                              (just-one-space))))))

    ;; Position point inside template braces.
    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _)
                        (when (smart-ops-after-match? (rx "<" (* space) ">"))
                          (search-backward ">")
                          (delete-horizontal-space))))

    ;; Inserting this op means you're probably adding a type annotation. Pad
    ;; internally and move point inside.
    (let ((inserting-type? (smart-ops-before-match? (rx bos (* space) "="))))
      (smart-ops ":="
                 :action
                 (lambda (&rest _)
                   (when (funcall inserting-type? (point))
                     (just-one-space)
                     (save-excursion
                       (insert " ")
                       (search-backward ":")
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
    (sp-with-modes 'rust-mode
      (sp-local-pair "{" "}" :post-handlers '(:add sp/internal-and-external-padding))
      (sp-local-pair "'" "'" :actions '(:rem insert))
      (sp-local-pair "<" ">" :actions '(:rem insert)))))

(provide 'packages)

;;; packages.el ends here
