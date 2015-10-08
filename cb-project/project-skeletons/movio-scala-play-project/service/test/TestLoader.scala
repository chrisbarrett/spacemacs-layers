package globals

import com.softwaremill.macwire.MacwireMacros._
import java.io.File
import play.api._
import play.api.ApplicationLoader.Context

object TestLoader {
  val classLoader = ApplicationLoader.getClass.getClassLoader
  val env = new Environment(new File("."), classLoader, Mode.Test)
  val testContext = ApplicationLoader.createContext(env)
}

class TestLoader extends Loader {
  import TestLoader._

  def loadTestApplication(): Application = load(testContext)

  override def load(context: Context): Application = {
    new BuiltInComponentsFromContext(context) with Components {

      // Override wired dependencies for tests here.

    }.application
  }
}
