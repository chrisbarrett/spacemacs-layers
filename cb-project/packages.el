;;; packages.el --- cb-project Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash)
  (require 'hydra nil t)
  (require 's)
  (require 'f)
  (require 'cb-vars)
  (require 'cb-use-package-extensions)
  (require 'use-package))

(defconst cb-project-packages
  '(skeletor
    projectile
    recentf
    helm-projectile
    ag
    helm-ag
    neotree
    (cb-project-show-project :location local)))

(defun cb-project/regexp-quoted-ignored-dirs ()
  (--map (format "/%s/" (regexp-quote it)) cb-vars-ignored-dirs))

(defun cb-project/user-config ()
  (setq projectile-completion-system 'helm)
  (with-eval-after-load 'projectile
    (projectile-cleanup-known-projects)))

(defun cb-project/post-init-recentf ()
  (use-package recentf
    :config
    (setq recentf-exclude (-union recentf-exclude (cb-project/regexp-quoted-ignored-dirs)))))

(defun cb-project/post-init-projectile ()
  (use-package projectile
    :bind
    (("s-f" . projectile-find-file)
     ("s-d" . projectile-find-dir)
     ("s-l" . spacemacs/helm-persp-switch-project))
    :leader-bind
    (("pa" . cb-projectile-toggle-between-implementation-and-test)
     ("pt" . projectile-test-project))
    :config
    (progn
      (setq projectile-enable-caching t)
      (setq projectile-switch-project-action #'magit-status)

      ;; Define a command that switches between test and impl, optionally in
      ;; another window.

      (defun cb-projectile-toggle-between-implementation-and-test (&optional arg)
        "Toggle between an implementation file and its test file."
        (interactive "P")
        (let ((file (projectile-find-implementation-or-test (buffer-file-name))))
          (if arg
              (find-file-other-window file)
            (find-file file))))

      ;;; Vars

      (setq projectile-ignored-projects '("/usr/local/"))
      (setq projectile-globally-ignored-directories cb-vars-ignored-dirs)

      (dolist (suf cb-vars-ignored-extensions)
        (add-to-list 'projectile-globally-ignored-file-suffixes suf))

      (defun cb-project--recentf-cleanup (_ &rest _) (recentf-cleanup))
      (advice-add #'projectile-invalidate-cache :before #'cb-project--recentf-cleanup)

      ;;; HACK: fix duplicates in projectile projects list on OS X.

      (with-eval-after-load 'projectile
        (defun projectile-relevant-known-projects ()
          "Return a list of known projects except the current one (if present)."
          (if (projectile-project-p)

              (->> projectile-known-projects
                   (--reduce-from
                    (if (-contains? (-map 's-downcase acc) (s-downcase it)) acc (cons it acc))
                    (list (abbreviate-file-name (projectile-project-root))))
                   (-sort 'string-lessp))

            projectile-known-projects)))

      ;; Customise project types.

      (defun cb-projectile-test-suffix (project-type)
        (cond
         ((member project-type '(emacs-cask)) "-test")
         ((member project-type '(rails-rspec ruby-rspec)) "_spec")
         ((member project-type '(rails-test ruby-test lein-test boot-clj go)) "_test")
         ((member project-type '(scons)) "test")
         ((member project-type '(maven symfony)) "Test")
         ((member project-type '(haskell-stack gradle gradlew grails)) "Spec")))

      (setq projectile-test-suffix-function #'cb-projectile-test-suffix))))

(defun cb-project/post-init-helm-projectile ()
  (use-package helm-projectile
    :after projectile
    :bind (("s-t" . helm-projectile))
    :leader-bind (("oo" . helm-occur))))

(defun cb-project/post-init-ag ()
  (use-package ag
    :after projectile
    :config
    (setq ag-ignore-list
          (->> (list
                ag-ignore-list
                cb-vars-ignored-files-regexps
                (cb-project/regexp-quoted-ignored-dirs))
               -flatten
               -uniq))))

(defun cb-project/post-init-helm-ag ()
  (use-package helm-ag
    :after projectile
    :config
    (progn
      (setq helm-ag-ignore-patterns
            (->> (list
                  helm-ag-ignore-patterns
                  cb-vars-ignored-files-regexps
                  (cb-project/regexp-quoted-ignored-dirs))
                 -flatten
                 -uniq))

      (setq helm-ag-insert-at-point 'symbol))))

(defun cb-project/init-skeletor ()
  (use-package skeletor
    :commands (skeletor-create-project skeletor-create-project-at)
    :init
    (eval-and-compile
      (defconst cb-project/scala-artifact-versions
        '(("__SCALA-VERSION__"         . "2.11.7")
          ("__SBT-VERSION__"           . "0.13.8")

          ("__ESDOMAINLIB-VERSION__"   . "6.0.0")
          ("__ELASTIC4S-VERSION__"     . "1.4.12")
          ("__JODA-CONVERT-VERSION__"  . "1.7")
          ("__JODA-TIME-VERSION__"     . "2.4")
          ("__MACWIRE-VERSION__"       . "1.0.5")
          ("__PLAY-VERSION__"          . "2.4.3")
          ("__PLAYLIB-VERSION__"       . "3.0.0")
          ("__SBT-RELEASE-VERSION__"   . "0.8.5")
          ("__SCALATESTPLUS-VERSION__" . "1.4.0-M4")
          ("__SCALATEST-VERSION__"     . "2.2.4")
          )))
    :config
    (progn
      (setq skeletor-show-project-command #'magit-status)
      (setq skeletor-scala-use-ensime t)
      (setq skeletor-user-directory (f-join user-layers-directory "cb-project/project-skeletons"))

      (skeletor-define-constructor "MM Microservice"
        :title "MM Microservice (Scala, Play 2.4)"
        :no-git? t
        :no-license? t
        :requires-executables '(("g8" . "https://github.com/n8han/giter8"))
        :initialise
        (lambda (spec)
          (let-alist spec
            (let ((template-dir "/Users/chrisb/.g8/mm-microservice.g8"))
              (skeletor--log-info "Updating g8 template...")
              (skeletor-shell-command "git fetch origin; git stash save; git reset --hard origin/master" template-dir)
              (skeletor--log-info "Generating project...")
              (skeletor-with-shell-setup (format "g8 file://%s --name=%s"
                                                 (shell-quote-argument template-dir)
                                                 (shell-quote-argument .project-name))
                                         #'ignore
                                         .project-dir)))))

      (skeletor-define-template "movio-scala-library"
        :title "MM Library (Scala)"
        :no-license? t
        :requires-executables
        '(("scala" . "http://www.scala-lang.org")
          ("sbt" . "http://www.scala-sbt.org"))

        :substitutions
        `(,@cb-project/scala-artifact-versions
          ("__MOVIO-ARTIFACTORY-URL__". movio-artifactory-url)
          ("__MOVIO-ARTIFACTORY-REPO-URL__". movio-artifactory-repo-url)
          ("__DESCRIPTION__" . (lambda () (read-string "Description: "))))

        :after-creation
        (lambda (dir)
          (when skeletor-scala-use-ensime
            (skeletor--log-info "Configuring SBT and ENSIME. This may take a while...")
            (sbt-gen-ensime dir)))))))

(defun cb-project/post-init-neotree ()
  (use-package neotree
    :defer t
    :config
    (progn
      (cb-remap-face 'neo-dir-link-face 'default)
      (set-face-foreground neo-file-link-face cb-vars-solarized-hl-orange)
      (set-face-foreground neo-root-dir-face cb-vars-solarized-hl-blue))))

(defun cb-project/init-cb-project-show-project ()
  (use-package cb-project-show-project
    :disabled t
    :after projectile
    :config (cb-project-show-project-init)))

;;; packages.el ends here
