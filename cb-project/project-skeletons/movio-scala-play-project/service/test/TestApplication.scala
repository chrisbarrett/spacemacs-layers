package globals

import org.scalatest.Suite
import org.scalatestplus.play.OneAppPerSuite

trait TestApplication extends OneAppPerSuite { this: Suite ⇒
  override implicit lazy val app = new TestLoader().loadTestApplication()
}
