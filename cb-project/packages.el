;;; packages.el --- cb-project Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-project-packages
  '(
    projectile
    skeletor
    helm-projectile
    neotree
    ag
    helm-ag
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defconst cb-project-excluded-packages '()
  "List of packages to exclude.")

(eval-when-compile
  (require 'use-package nil t)
  (require 'dash nil t)
  (require 's nil t))

(with-eval-after-load 'recentf
  (setq recentf-exclude (-union recentf-exclude (cb-core/regexp-quoted-ignored-dirs))))

(defun cb-project/init-projectile ()
  (use-package projectile
    :bind
    (("s-f" . projectile-find-file)
     ("s-F" . project/find-file-in-scope)
     ("s-d" . projectile-find-dir)
     ("s-l" . projectile-switch-project))
    :init
    (evil-leader/set-key
      "pa" 'projectile-ag
      "pg" 'projectile-find-other-file
      "pG" 'projectile-find-other-file-other-window)
    :config
    (progn
      ;;; Vars

      (setq projectile-ignored-projects '("/usr/local/"))
      (setq projectile-switch-project-action (lambda ()
                                               (projectile-invalidate-cache nil)
                                               (call-interactively 'magit-status)))
      (setq projectile-globally-ignored-directories cb-core/ignored-dirs)

      (defadvice projectile-invalidate-cache (before recentf-cleanup activate)
        (recentf-cleanup))

      (add-hook 'after-init-hook
                (lambda ()
                  (setq projectile-completion-system 'helm)
                  (projectile-cleanup-known-projects)))

      ;; Advice

      (defadvice projectile-cache-current-file (around ignore-errors activate)
        (ignore-errors ad-do-it))

      (defadvice projectile-replace (around save-window-excursion activate)
        (save-window-excursion ad-do-it)))))

(defun cb-project/init-helm-projectile ()
  (use-package helm-projectile
    :bind
    (("s-t" . helm-projectile))))

(defun cb-project/init-ag ()
  (use-package ag
    :defer t
    :config
    (setq ag-ignore-list (-union ag-ignore-list (cb-core/regexp-quoted-ignored-dirs)))))

(defun cb-project/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
    (setq helm-ag-insert-at-point 'symbol)))

(defun cb-project/init-skeletor ()
  (use-package skeletor
    :commands (skeletor-create-project skeletor-create-project-at)
    :init
    (eval-and-compile
      (defconst cb-project/scala-artifact-versions
        '(("__SCALA-VERSION__"          . "2.11.7")
          ("__SBT-VERSION__"            . "0.13.8")

          ("__ELASTIC4S-VERSION__"      . "1.4.12")
          ("__JODA-CONVERT-VERSION__"   . "1.7")
          ("__JODA-TIME-VERSION__"      . "2.4")
          ("__MACWIRE-VERSION__"        . "1.0.5")
          ("__PLAY-VERSION__"           . "2.3.9")
          ("__PLAYLIB-VERSION__"        . "1.1.0")
          ("__SBT-RELEASE-VERSION__"    . "0.8.5")
          ("__SCALATEST-PLUS-VERSION__" . "1.1.0")
          ("__SCALATEST-VERSION__"      . "2.2.4")
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
          ("__MOVIO-ARTIFACTORY-URL__". movio-artifactory-url)
          ("__MOVIO-ARTIFACTORY-REPO-URL__". movio-artifactory-repo-url)

          ("__DESCRIPTION__" . (lambda () (read-string "Description: ")))
          ("__ENDPOINT-PATH__" .
           (lambda ()
             (let ((path (concat "/" (read-string "Endpoint path: /"))))
               (setq cb-project/scala-movio-endpoint-test-prefix path)
               path)))
          ("__ENDPOINT-VERB__" . (lambda () (read-string "HTTP verb: " "GET")))
          ("__ENDPOINT-TEST-PREFIX__" .
           (lambda ()
             (let ((default
                     (concat (s-join "" (-map 's-capitalize (s-split "/" cb-project/scala-movio-endpoint-test-prefix t)))
                             "Endpoint")))
               (read-string "Endpoint test class prefix: " default))))
          ("__APIDOC-PROJECT-NAME__" .(lambda () (s-chop-suffix "-svc" (s-chop-prefix "mm-" skeletor-project-name))))
          ("__ENDPOINT-HANDLER__" . (lambda () (s-lower-camel-case (read-string "Handler method name: "))))
          ("__CONTROLLER-NAME__" . (lambda () (s-upper-camel-case (s-chop-suffix ".scala" (read-string "Controller class name: "))))))

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

(use-package neotree
  :defer t
  :config
  (progn
    (core/remap-face 'neo-dir-link-face 'default)
    (set-face-foreground neo-file-link-face solarized-hl-orange)
    (set-face-foreground neo-root-dir-face solarized-hl-blue)))
