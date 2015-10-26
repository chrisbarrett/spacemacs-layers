package application

import mm.es.entities.{ EsAlias, EsMapping }
import com.typesafe.config.ConfigFactory

case class ServiceConfig(
  elasticsearch: EsConfiguration
)

case class EsConfiguration(
  memberAlias:   EsAlias,
  memberMapping: EsMapping,
  movieAlias:    EsAlias,
  movieMapping:  EsMapping
)

object ServiceConfig {
  def load(): ServiceConfig = ConfigFactory.load.get[ServiceConfig]
}
