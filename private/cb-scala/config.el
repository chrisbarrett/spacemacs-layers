(setq flycheck-scalastyle-jar "~/bin/scalastyle/lib/scalastyle_2.10-0.5.0.jar")
(setq flycheck-scalastylerc "~/bin/scalastyle/scalastyle_config.xml")
(setq flycheck-scala-scalastyle-executable "~/bin/scalastyle/scalastyle-batch_2.10.jar")
(setq ensime-default-scala-version "2.11.5")

(add-to-list 'core/indent-commands-alist '(scala-mode . ensime-format-source))
