(setq flycheck-scalastyle-jar "~/bin/scalastyle/lib/scalastyle_2.10-0.5.0.jar")
(setq flycheck-scalastylerc "~/bin/scalastyle/scalastyle_config.xml")
(setq flycheck-scala-scalastyle-executable "~/bin/scalastyle/scalastyle-batch_2.10.jar")

(with-eval-after-load 'popwin
  (push '("^\\*sbt\\*" :regexp t :dedicated t :position bottom :stick t :noselect nil :height 33) popwin:special-display-config))
