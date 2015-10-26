package globals

import play.api.{ Configuration, Environment }
import play.api.inject.Module

class AppModule extends Module {

  def bindings(env: Environment, conf: Configuration) = {
    val config = configuration.ServiceConfig.loadConfig()

    Seq(
      bind[ServiceConfig].toInstance(config)
    )
  }
}
