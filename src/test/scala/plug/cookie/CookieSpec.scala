package plug.cookie

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{FlatSpec, Matchers}
import plug.Uri

/**
  * Created by arne on 1/5/17.
  */
class CookieSpec extends FlatSpec with Matchers {

  "formatCookieDateTimeString" should "convert datetime to cookie date string" in {
    val dt = new DateTime(2010, 12, 31, 23, 59, 59, DateTimeZone.UTC)
    val actual = Cookie.formatCookieDateTimeString(dt)
    actual should equal("Fri, 31-Dec-2010 23:59:59 GMT")
  }

  "Cookie" should "be expired if expires date is in the past" in {
    val cookie = Cookie("a", "xyz", expiresOrMaxAge = Some(Left(DateTime.now().minusDays(1))))
    cookie.expired shouldBe true
  }

  "validateCookieForUri" should "add domain to cookie if it has none" in {
    val cookie = Cookie("a", "b", path = Some("/foo"))
    val uri = Uri.fromString("http://example.com/foo/bar").get
    val expected = Some(Cookie("a", "b", domain = Some("example.com"), path = Some("/foo")))

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(expected)
  }

  it should "add path to cookie if it has none" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"))
    val uri = Uri.fromString("http://bar.example.com/foo").get
    val expected = Some(Cookie("a", "b", domain = Some("example.com"), path = Some("/foo"), hostOnly = true))

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(expected)
  }

  it should "return None for a cookie with the wrong domain" in {
    val cookie = Cookie("a", "b", domain = Some("example.org"))
    val uri = Uri.fromString("http://example.com/foo/bar").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(None)
  }

  it should "return None for request Host that is not a subdomain of the cookie Domain" in {
    val cookie = Cookie("a", "b", domain = Some("foo.example.com"))
    val uri = Uri.fromString("http://example.com/foo/bar").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(None)
  }

  it should "return the same cookie if request Host is a subdomain of the cookie Domain" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"), path = Some("/"))
    val uri = Uri.fromString("http://foo.example.com/").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(Some(cookie))
  }

  it should "return the same cookie if request Host is the same as the cookie Domain" in {
    val cookie = Cookie("a", "b", domain = Some("foo.example.com"), path = Some("/"))
    val uri = Uri.fromString("http://foo.example.com/").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(Some(cookie))
  }

  it should "ignore case in domain" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"), path = Some("/"))
    val uri = Uri.fromString("http://eXample.Com/").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(Some(cookie))
  }

  it should "add uri path to cookie is cookie's path is empty" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"))
    val uri = Uri.fromString("http://example.com/foo/bar").get
    val expected = Some(Cookie("a", "b", domain = Some("example.com"), path = Some("/foo/bar")))
    val actual = cookie.validateCookieForUri(uri)

    actual should equal(expected)
  }

  it should "leave cookie unchanged if it has a non-empty path" in {
    val cookie = Cookie("a", "b", domain = Some("example.com"), path = Some("/foo/bar"))
    val uri = Uri.fromString("http://example.com/foo").get

    val actual = cookie.validateCookieForUri(uri)

    actual should equal(Some(cookie))
  }
}
