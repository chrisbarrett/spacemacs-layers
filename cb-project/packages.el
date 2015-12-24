;;; packages.el --- cb-project Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t)
  (require 'dash nil t)
  (require 'hydra nil t)
  (require 's nil t))

(defconst cb-project-packages
  '(skeletor
    projectile
    recentf
    helm-projectile
    ag
    helm-ag
    neotree))

(defun cb-project/user-config ()
  (setq projectile-completion-system 'helm)
  (projectile-cleanup-known-projects))

(defun cb-project/post-init-recentf ()
  (with-eval-after-load 'recentf
    (setq recentf-exclude (-union recentf-exclude (cb-core/regexp-quoted-ignored-dirs)))))

(defun cb-project/post-init-projectile ()
  (use-package projectile
    :config
    (progn
      ;;; Define a command that switches between test and impl, optionally in
      ;;; another window.

      (defun cb-projectile-toggle-between-implementation-and-test (&optional arg)
        "Toggle between an implementation file and its test file."
        (interactive "P")
        (let ((file (projectile-find-implementation-or-test (buffer-file-name))))
          (if arg
              (find-file-other-window file)
            (find-file file))))

      (evil-leader/set-key "pa" 'cb-projectile-toggle-between-implementation-and-test)
      (evil-leader/set-key "pt" 'projectile-test-project)

      ;;; Use a hydra picker for actions when switching project.
      ;;;
      ;;; HACK: Define utilities and a macro to rebind `default-directory' for
      ;;; hydra commands. This is needed for those commands to operate in the
      ;;; project being switched to.

      (defvar cb-project/project-to-switch)

      (defun cb-project/set-project-to-switch ()
        (setq cb-project/project-to-switch default-directory))

      (add-hook 'projectile-before-switch-project-hook 'cb-project/set-project-to-switch)

      (defmacro cb-project/with-project-as-default-directory (&rest body)
        (declare (indent 0))
        `(let* ((default-directory cb-project/project-to-switch)
                (it default-directory))
           ,@body))

      (defhydra cb-project-show-project (:color blue)
        "Execute in project"
        ("/" (cb-project/with-project-as-default-directory (helm-projectile-ag)) "ag")
        ("d" (cb-project/with-project-as-default-directory (dired it)) "dired")
        ("e" (cb-project/with-project-as-default-directory (cb-eshell--new)) "eshell")
        ("f" (cb-project/with-project-as-default-directory (projectile-find-file)) "find file")
        ("g" (cb-project/with-project-as-default-directory (magit-status it)) "magit status")
        ("S" (cb-project/with-project-as-default-directory (sbt it)) "SBT")
        ("q" nil "cancel"))

      ;;; Vars

      (setq projectile-ignored-projects '("/usr/local/"))
      (setq projectile-switch-project-action 'cb-project-show-project/body)
      (setq projectile-globally-ignored-directories cb-core/ignored-dirs)

      (defadvice projectile-invalidate-cache (before recentf-cleanup activate)
        (recentf-cleanup))

      ;; Advice

      (defadvice projectile-cache-current-file (around ignore-errors activate)
        (ignore-errors ad-do-it))

      (defadvice projectile-replace (around save-window-excursion activate)
        (save-window-excursion ad-do-it))

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

      (defun projectile-test-suffix (project-type)
        "Find default test files suffix based on PROJECT-TYPE."
        (cond
         ((member project-type '(rails-rspec ruby-rspec)) "_spec")
         ((member project-type '(rails-test ruby-test lein-test boot-clj go)) "_test")
         ((member project-type '(scons)) "test")
         ((member project-type '(maven symfony)) "Test")
         ((member project-type '(haskell-stack gradle gradlew grails)) "Spec"))))))

(defun cb-project/post-init-helm-projectile ()
  (use-package helm-projectile
    :bind
    (("s-t" . helm-projectile))))

(defun cb-project/post-init-ag ()
  (with-eval-after-load 'ag
    (setq ag-ignore-list (-union ag-ignore-list (cb-core/regexp-quoted-ignored-dirs)))))

(defun cb-project/post-init-helm-ag ()
  (setq helm-ag-insert-at-point 'symbol))

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
      (setq skeletor-show-project-command 'magit-status)
      (setq skeletor-scala-use-ensime t)
      (setq skeletor-user-directory (f-join user-layers-directory "cb-project/project-skeletons"))

      (defvar cb-project/scala-movio-endpoint-test-prefix)

      (skeletor-define-template "movio-scala-play-project"
        :title "Scala Play Project (Movio)"
        :no-license? t
        :requires-executables
        '(("scala" . "http://www.scala-lang.org")
          ("sbt" . "http://www.scala-sbt.org"))

        :substitutions
        `(,@cb-project/scala-artifact-versions
          ("__MOVIO-ARTIFACTORY-URL__" . movio-artifactory-url)
          ("__MOVIO-ARTIFACTORY-REPO-URL__" . movio-artifactory-repo-url)

          ("__APIDOC-PROJECT-NAME__" .
           (lambda () (read-string "Apidoc project name: " (s-chop-suffix "-svc" (s-chop-prefix "mm-" skeletor-project-name)))))

          ("__DESCRIPTION__" . (lambda () (read-string "Description: "))))

        :after-creation
        (lambda (dir)
          (when skeletor-scala-use-ensime
            (skeletor--log-info "Configuring SBT and ENSIME. This may take a while...")
            (sbt-gen-ensime dir))))


      (skeletor-define-template "movio-scala-library"
        :title "Scala Library (Movio)"
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
  (with-eval-after-load 'neotree
    (core/remap-face 'neo-dir-link-face 'default)
    (set-face-foreground neo-file-link-face solarized-hl-orange)
    (set-face-foreground neo-root-dir-face solarized-hl-blue)))
