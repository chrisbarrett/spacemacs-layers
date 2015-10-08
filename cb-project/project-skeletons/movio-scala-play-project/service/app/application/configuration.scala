package application

import mm.es.entities.{ EsAlias, EsMapping }
import com.typesafe.config.ConfigFactory

case class Configuration(
  elasticsearch: EsConfiguration
)

case class EsConfiguration(
  memberAlias:   EsAlias,
  memberMapping: EsMapping,
  movieAlias:    EsAlias,
  movieMapping:  EsMapping
)

object Configuration {
  def load(): Configuration = {
    val config = ConfigFactory.load

    val es = config.getConfig("elasticsearch")
    val esConfig =
      EsConfiguration(
        memberAlias = EsAlias(es.getString("memberAlias")),
        memberMapping = EsMapping(es.getString("memberMapping")),
        movieAlias = EsAlias(es.getString("movieAlias")),
        movieMapping = EsMapping(es.getString("movieMapping"))
      )

    Configuration(elasticsearch = esConfig)
  }
}
