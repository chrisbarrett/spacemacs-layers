package controllers

import mm.play.controllers._
import mm.play.controllers.security.JwtAuthenticated
import play.api.libs.json._
import play.api.mvc._

class __CONTROLLER-NAME__ extends Controller with ControllerHelpers {
  implicit val endpoint = EndpointName("__ENDPOINT-PATH__")

  def __ENDPOINT-HANDLER__() = JwtAuthenticated.async { request ⇒
    ???
  }
}
