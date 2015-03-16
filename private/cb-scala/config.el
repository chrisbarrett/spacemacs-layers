(setq flycheck-scalastyle-jar "~/bin/scalastyle/lib/scalastyle_2.10-0.5.0.jar")
(setq flycheck-scalastylerc "~/bin/scalastyle/scalastyle_config.xml")
(setq flycheck-scala-scalastyle-executable "~/bin/scalastyle/scalastyle-batch_2.10.jar")

(add-to-list 'core/indent-commands-alist '(scala-mode . ensime-format-source))
(add-hook 'sbt-mode-hook 'turn-off-show-smartparens-mode)
(add-hook 'sbt-mode-hook (lambda () (show-paren-mode -1)))
