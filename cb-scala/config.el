(with-eval-after-load 'popwin
  (push '("^\\*sbt\\*" :regexp t :dedicated t :position bottom :stick t :noselect nil :height 33) popwin:special-display-config))
