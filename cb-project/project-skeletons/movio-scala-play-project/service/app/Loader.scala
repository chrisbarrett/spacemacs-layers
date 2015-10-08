package globals

import com.softwaremill.macwire._
import controllers.Assets
import play.api.ApplicationLoader.Context
import play.api._
import play.api.routing.Router
import router.Routes

class Loader extends ApplicationLoader {
  def load(context: Context): Application = {
    Logger.configure(context.environment)
    val components = new BuiltInComponentsFromContext(context) with Components
    components.application
  }
}

trait Components extends BuiltInComponents with Module {
  lazy val assets: Assets = wire[Assets]
  lazy val router: Router = wire[Routes] withPrefix "/"
}
