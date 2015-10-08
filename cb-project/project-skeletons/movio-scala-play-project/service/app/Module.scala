package globals

import com.softwaremill.macwire._

trait Module extends Macwire {
  lazy val config = application.Configuration.load()
  lazy val esConfig = config.elasticsearch
  lazy val newService = wire[services.NewService]
  lazy val newController = wire[controllers.NewController]
}
