package application

import com.softwaremill.macwire._
import com.typesafe.config.ConfigFactory

trait ApplicationModule extends Macwire {
  lazy val config = ConfigFactory.load
  lazy val __(s-lower-camel-case "__CONTROLLER-NAME__")__ = wire[controllers.__CONTROLLER-NAME__]
}
