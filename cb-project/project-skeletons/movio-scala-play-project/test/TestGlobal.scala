package application

import com.softwaremill.macwire._
import play.api.GlobalSettings

object TestApplication extends ApplicationModule {
  // Override dependencies for integration tests here.
}

object TestGlobal extends GlobalSettings with Macwire {
  val wired = wiredInModule(TestApplication)

  override def getControllerInstance[A](controllerClass: Class[A]) =
    wired.lookupSingleOrThrow(controllerClass)
}
