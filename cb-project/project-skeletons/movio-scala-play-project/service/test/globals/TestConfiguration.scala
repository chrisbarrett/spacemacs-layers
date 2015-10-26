package globals

trait TestConfiguration {
  lazy val config = configuration.ServiceConfig.loadConfig()
}
