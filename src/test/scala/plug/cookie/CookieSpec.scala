package plug.cookie

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{FlatSpec, Matchers}

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
    val cookie = Cookie("a", "xyz", expires = Some(DateTime.now().minusDays(1)))
    cookie.expired shouldBe true
  }

  "Internals.toSegments" should "parse '/' into empty list" in {
    Cookie.Internals.toSegments(Some("/")) should equal(Some(Nil))
  }
}
