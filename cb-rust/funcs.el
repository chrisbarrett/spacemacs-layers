(defun rust/set-rust-library-path ()
  "Set the search path for rust libraries."
  (require 'flycheck)
  (add-to-list 'flycheck-rust-library-path ".")
  (when (projectile-project-p)
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "src"))
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "lib"))))
