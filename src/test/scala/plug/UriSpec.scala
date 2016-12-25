package plug

import org.scalatest._

/**
  * Created by arne on 12/24/16.
  */
class UriSpec extends FlatSpec with Matchers {

  val baseUri = Uri.fromString("http://example.com")

  "encode" should "convert char é to several encoded elements %C3%A9" in {
    Uri.encode("créate", UriEncoding.Default) should equal("cr%C3%A9ate")
  }

  it should "convert kanji 常 to several encoded elements" in {
    Uri.encode("常", UriEncoding.Default) should equal("%E5%B8%B8")
  }

  "encodeUserInfo" should "replace space with +" in {
    Uri.encodeUserInfo("foo bar") should equal("foo+bar")
  }

  it should "replace : with %3A" in {
    Uri.encodeUserInfo("foo:bar") should equal("foo%3Abar")
  }

  "toUriString" should "replace password when requested" in {
    Uri.fromString("http://foo:bar@baz.com").get.toUriString(false) should equal("http://foo:xxx@baz.com")
  }

  "path" should "contain trailing slash if trailingSlash is set" in {
    Uri("http", "example.com", segments = List("a", "bb", "ccc"), trailingSlash = true).path should equal("/a/bb/ccc/")
  }

  it should "not contain trailing slash if trailingSlash is not set" in {
    Uri("http", "example.com", segments = List("a", "bb", "ccc")).path should equal("/a/bb/ccc")
  }

  "query" should "return none if params are None" in {
    Uri("http", "example.com").query should equal(None)
  }

  it should "return empty string if params are empty" in {
    Uri("http", "example.com", params = Some(Nil)).query should equal(Some(""))
  }

  it should "render params" in {
    Uri("http", "example.com", params = Some(List("foo" -> None, "" -> None, "bar" -> Some("baz")))).query should equal(
      Some("foo&&bar=baz")
    )
  }
}
