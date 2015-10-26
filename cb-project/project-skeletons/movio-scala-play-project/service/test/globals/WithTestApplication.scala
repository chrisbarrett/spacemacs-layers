package globals

import configuration.ServiceConfig
import org.scalatest.Suite
import org.scalatestplus.play.OneAppPerSuite
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder

class Overrides extends MockitoSugar with ElasticSugar with TestConfiguration {
  // Override test dependencies here
}

trait WithTestApplication extends OneAppPerSuite { this: Suite ⇒
  override implicit lazy val app: Application = {
    val overrides = new Overrides()
    new GuiceApplicationBuilder()
      .overrides(
        bind[ServiceConfig].toInstance(overrides.config)
      )
      .build
  }
}
