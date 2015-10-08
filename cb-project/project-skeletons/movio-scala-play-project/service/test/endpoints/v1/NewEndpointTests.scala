package endpoints.v1

import org.scalatest.{ Matchers, WordSpec }
import mm.play.test.jwt.AuthenticatedFakeRequest
import play.api.test.Helpers._
import play.api.test._

trait NewEndpointTest extends WordSpec with Matchers with globals.TestApplication {
  val Endpoint = "/hello"
}

class NewEndpoint_NotAuthenticated extends NewEndpointTest {
  lazy val Some(response) = route(FakeRequest(GET, Endpoint))

  "GET" when {
    "not authenticated" should {
      "return 401 Unauthorized" in {
        status(response) shouldBe UNAUTHORIZED
      }
    }
  }
}

class NewEndpoint_Authenticated extends NewEndpointTest {
  val payload = """{ "email":"someone@movio.co", "app_metadata": { "roles": [] }}"""
  lazy val Some(response) = route(AuthenticatedFakeRequest(GET, Endpoint, payload))

  "GET" when {
    "valid credentials" should {
      "return 200 OK" in {
        status(response) shouldBe OK
      }
    }
  }
}
