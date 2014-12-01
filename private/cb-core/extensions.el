(defvar cb-core-pre-extensions
  '(
    ;; pre extension cores go here
    super-smart-ops
    file-template
    )
  "List of all extensions to load before the packages.")

(defvar cb-core-post-extensions
  '(
    ;; post extension cores go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function core/init-<extension-core>
;;
;; (defun core/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun core/init-super-smart-ops ()
  (use-package super-smart-ops))

(defun core/init-file-template ()
  (use-package file-template
    :defer t
    :commands file-template-find-file-not-found-hook
    :init
    (add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook t)
    :config
    (progn
      (require 'f)
      (setq core/file-templates-dir (f-join spacemacs-private-directory
                                            "cb-core/extensions/file-template"
                                            "templates"))
      (setq file-template-insert-automatically t)
      (setq file-template-paths (list core/file-templates-dir))
      (setq file-template-mapping-alist
            (->> (f-files core/file-templates-dir)
              (-map 'f-filename)
              (--map (cons (format "\\.%s$" (f-ext it)) it))))

      (add-hook 'file-template-insert-hook 'core/reset-buffer-undo-history))))
