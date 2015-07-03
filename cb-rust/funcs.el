;;; Smart operators

(defun rust/smart-colon ()
  "Insert a colon as a smart operator.
Collapse spaces if this is a double-colon."
  (interactive "*")
  (super-smart-ops-insert ":")
  (save-excursion
    (when (search-backward-regexp (rx (* space) ":" (* space) ":" (* space))
                                  nil t)
      (replace-match "::")
      (search-backward "::")
      (delete-horizontal-space))))

(defun rust/set-rust-library-path ()
  "Set the search path for rust libraries."
  (require 'flycheck)
  (add-to-list 'flycheck-rust-library-path ".")
  (when (projectile-project-p)
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "src"))
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "lib"))))
