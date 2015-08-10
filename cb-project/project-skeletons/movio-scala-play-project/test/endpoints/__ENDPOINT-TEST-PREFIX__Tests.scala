package endpoints

import org.scalatest.{ Matchers, WordSpec }
import org.scalatestplus.play.OneServerPerSuite
import mm.play.test.jwt.AuthenticatedFakeRequest
import play.api.test.Helpers._
import play.api.test._

trait __ENDPOINT-TEST-PREFIX__Tests extends WordSpec with Matchers with OneServerPerSuite {
  override lazy val app = new FakeApplication(withGlobal = Some(application.TestGlobal))
  val Endpoint = "__ENDPOINT-PATH__"
}

class __ENDPOINT-TEST-PREFIX___NotAuthenticated extends __ENDPOINT-TEST-PREFIX__Tests {
  lazy val Some(response) = route(FakeRequest(__ENDPOINT-VERB__, Endpoint))

  s"__ENDPOINT-VERB__ $Endpoint" when {
    "not authenticated" should {
      "return 401 Unauthorized" in {
        status(response) shouldBe UNAUTHORIZED
      }
    }
  }
}

class __ENDPOINT-TEST-PREFIX___Authenticated extends __ENDPOINT-TEST-PREFIX__Tests {
  val payload = """{ "email":"someone@movio.co", "app_metadata": { "roles": [] }}"""
  lazy val Some(response) = route(AuthenticatedFakeRequest(__ENDPOINT-VERB__, Endpoint, payload))

  s"__ENDPOINT-VERB__ $Endpoint" when {
    "valid credentials" should {
      "return 200 OK" in {
        status(response) shouldBe OK
      }
    }
  }
}
