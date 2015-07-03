package endpoints

import org.scalatest.{ Matchers, WordSpec }
import org.scalatestplus.play.OneServerPerSuite
import mm.play.test.jwt.AuthenticatedFakeRequest
import play.api.test.Helpers._
import play.api.test._

class EndpointTests extends WordSpec with Matchers with OneServerPerSuite {
  override lazy val app = new FakeApplication(withGlobal = Some(application.TestGlobal))

  "__ENDPOINT-VERB__ __ENDPOINT-PATH__" when {
    val endpoint = "__ENDPOINT-PATH__"

    "not authenticated" should {
      "return 401 Unauthorized" in {
        val Some(response) = route(FakeRequest(GET, endpoint))
        status(response) shouldBe UNAUTHORIZED
      }
    }

    "valid credentials" should {
      val payload = """{ "email":"someone@movio.co", "app_metadata": { "roles": [] }}"""

      "return 200 OK" in {
        val request = AuthenticatedFakeRequest(GET, endpoint, payload)
        val Some(response) = route(request)
        status(response) shouldBe OK
      }
    }
  }
}
