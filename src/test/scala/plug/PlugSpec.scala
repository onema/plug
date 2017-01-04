package plug

import org.scalatest._
import plug.CookieJar.Global.current

/**
  * Created by arne on 12/31/16.
  */
class PlugSpec extends  FlatSpec with Matchers {

  val baseUri = Uri.fromString("http://example.com").get
  "Plug" should "encapsulate uri" in {
    Plug(baseUri).uri should equal(baseUri)
  }
}
