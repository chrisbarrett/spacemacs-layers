(defvar cb-git-packages
  '(
    ;; package cb-gits go here
    git-gutter
    git-auto-commit-mode
    git-commit-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cb-git-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cb-git/init-<package-cb-git>
;;
;; (defun cb-git/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cb-git/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :diminish git-auto-commit-mode
    :init
    (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))))

(defun cb-git/init-git-commit-mode ()
  (use-package git-commit-mode
    :config
    (progn
      (add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
      (put 'git-commit-mode 'fill-column 72)

      (add-hook 'git-commit-mode-hook 'end-of-line)

      (defadvice git-commit-commit (after kill-commit-buffer activate)
          "Ensure the commit buffer is killed."
          (-when-let (buf (get-buffer "COMMIT_EDITMSG"))
            (when (buffer-live-p buf)
              (kill-buffer buf)))))))

(defun cb-git/init-git-gutter ()
  (use-package git-gutter
    :defer t
    :config
    (global-git-gutter-mode +1)))
