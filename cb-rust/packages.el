;;; packages.el --- cb-rust Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'skeletor nil t)
  )

(defconst cb-rust-packages
  '(smart-ops
    skeletor))

(defun cb-rust/post-init-smart-ops ()
  (define-smart-ops-for-mode 'rust-mode
    (smart-ops ";" "," "~" "&" ":" :pad-before nil)

    (smart-ops "." "::"
               :pad-before nil :pad-after nil
               :action #'company-manual-begin)

    ;; Position point inside template braces.
    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _) (search-backward ">")))

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

(provide 'packages)

;;; packages.el ends here
