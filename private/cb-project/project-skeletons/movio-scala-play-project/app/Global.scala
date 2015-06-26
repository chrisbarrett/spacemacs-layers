import play.api.GlobalSettings
import com.softwaremill.macwire._

object Global extends GlobalSettings with Macwire {
  val wired = wiredInModule(new application.ApplicationModule {})

  override def getControllerInstance[A](controllerClass: Class[A]) =
    wired.lookupSingleOrThrow(controllerClass)
}
