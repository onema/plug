package plug

import org.scalatest._

/**
  * Created by arne on 12/24/16.
  */
class UriSpec extends FlatSpec with Matchers {

  "encode" should "convert char é to several encoded elements %C3%A9" in {
    Uri.encode("créate",UriEncoding.Default) should equal("cr%C3%A9ate")
  }

  it should "convert kanji 常 to several encoded elements" in {
    Uri.encode("常",UriEncoding.Default) should equal("%E5%B8%B8")
  }

  "encodeUserInfo" should "replace space with +" in {
    Uri.encodeUserInfo("foo bar") should equal("foo+bar")
  }

  it should "replace : with %3A" in {
    Uri.encodeUserInfo("foo:bar") should equal("foo%3Abar")
  }


}
