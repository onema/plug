package plug.cookie

import org.scalatest.{FlatSpec, Matchers}
import plug.Uri

class CookieJarSpec extends FlatSpec with Matchers with TestSuffix {

  "checkCookieForUpdate" should "add domain to cookie if it has none" in {
    val cookie = Cookie("a", "b", path = Some("/foo"))
    val uri = Uri.fromString("http://example.com/foo/bar").get
    val expected = Some(Cookie("a", "b", domain = Some("example.com"), path = Some("/foo")))

    val actual = CookieJar.checkCookieForUpdate(cookie, uri)

    actual should equal(expected)
  }

  it should "add path to cookie if it has none" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"))
    val uri = Uri.fromString("http://bar.example.com/foo").get
    val expected = Some(Cookie("a", "b", domain = Some("example.com"), path = Some("/foo"), hostOnly = true))

    val actual = CookieJar.checkCookieForUpdate(cookie, uri)

    actual should equal(expected)
  }

  it should "return None for a cookie with the wrong domain" in {
    fail("not implemented")
  }

  it should "return None for request Host that is not a subdomain of the cookie Domain" in {
    fail("not implemented")
  }

  it should "ignore case in domain" in {
    fail("not implemented")
  }

  it should "return None for a request Path taht is not a subdir of the cookie Path" in {
    fail("not implemented")
  }

  it should "not ignore case in path" in {
    fail("not implemented")
  }
}
