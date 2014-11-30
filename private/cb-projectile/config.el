(require 'f)

(custom-set-variables
 '(projectile-known-projects-file (f-join cb:tmp-dir "projectile-bookmarks.eld"))
 '(projectile-cache-file (f-join spacemacs-cache-directory "projectile.cache"))
 '(projectile-ignored-projects '("/usr/local/"))
 '(projectile-switch-project-action (lambda () (call-interactively 'magit-status)))
 '(projectile-globally-ignored-directories
   '(".cask"
     ".cabal-sandbox"
     "dist"
     ".idea"
     ".eunit"
     ".git"
     ".hg"
     ".fslckout"
     ".bzr"
     "_darcs"
     ".tox"
     ".svn"
     "snippets"
     "build")))
