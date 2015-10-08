package controllers

import mm.play.controllers._
import mm.play.controllers.security.JwtAuthenticated
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global

class NewController(service: services.NewService) extends Controller {
  def hello() = JwtAuthenticated.async { request ⇒
    service.hello()
      .map { x ⇒ Ok(Json.toJson(x)) }
  }
}
