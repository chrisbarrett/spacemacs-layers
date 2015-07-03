(defun git/add-this-file ()
  "Run 'git add' on the file for the current buffer."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "Buffer has no corresponding file"))
   ((not (vc-git-root (buffer-file-name)))
    (user-error "Not a git repository"))
   ((yes-or-no-p "Stage all changes to this file?")
    (save-buffer)
    (vc-git-register (list (buffer-file-name)))
    (message "Done.")))
  (git-gutter))
