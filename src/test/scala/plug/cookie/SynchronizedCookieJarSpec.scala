package plug.cookie

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}
import plug.Uri

class SynchronizedCookieJarSpec extends FlatSpec with Matchers with TestSuffix {

  "get" should "retrieve all cookies that match partial path and exact domain" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(Cookie("example.com", "")), Map(
          "a" -> PathTree(List(Cookie("a", "")), Map(
            "b" -> PathTree(List(Cookie("a/b", "")), Map(
              "c" -> PathTree(List(Cookie("a/b/c", "")), Map(
                "d" -> PathTree(List(Cookie("a/b/c/d", "")), Map.empty)
              )),
              "c1" -> PathTree(List(Cookie("a/b/c1", "")), Map.empty)
            )),
            "b1" -> PathTree(List(Cookie("a/b1", "")), Map.empty)
          )),
          "a1" -> PathTree(List(Cookie("a1", "")), Map.empty)
        ))), Map.empty),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com", "a", "a/b", "a/b/c"))
  }

  it should "retrieve all cookies that match path and domain and parent domains" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(Cookie("example.com", "")), Map.empty)), Map(
          "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.example.com", "")), Map.empty)), Map(
            "bar" -> DomainTree(Some(PathTree(List(Cookie("bar.foo.example.com", "")), Map.empty)), Map.empty)
          ))
        )),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("http://foo.example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com", "foo.example.com"))
  }

  it should "filter secure cookies for http" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(Cookie("example.com", "", secure = true)), Map(
          "a" -> PathTree(List(Cookie("a", "")), Map(
            "b" -> PathTree(List(Cookie("a/b", "", secure = true)), Map(
              "c" -> PathTree(List(Cookie("a/b/c", "")), Map(
                "d" -> PathTree(List(Cookie("a/b/c/d", "")), Map.empty)
              )),
              "c1" -> PathTree(List(Cookie("a/b/c1", "")), Map.empty)
            )),
            "b1" -> PathTree(List(Cookie("a/b1", "")), Map.empty)
          )),
          "a1" -> PathTree(List(Cookie("a1", "")), Map.empty)
        ))), Map.empty),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("a", "a/b/c"))
  }

  it should "not filter secure cookies for https" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(Cookie("example.com", "", secure = true)), Map(
          "a" -> PathTree(List(Cookie("a", "")), Map(
            "b" -> PathTree(List(Cookie("a/b", "", secure = true)), Map(
              "c" -> PathTree(List(Cookie("a/b/c", "")), Map(
                "d" -> PathTree(List(Cookie("a/b/c/d", "")), Map.empty)
              )),
              "c1" -> PathTree(List(Cookie("a/b/c1", "")), Map.empty)
            )),
            "b1" -> PathTree(List(Cookie("a/b1", "")), Map.empty)
          )),
          "a1" -> PathTree(List(Cookie("a1", "")), Map.empty)
        ))), Map.empty),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("https://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com", "a", "a/b", "a/b/c"))
  }

  it should "filter expired cookies" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(
          Cookie("expired", "", expiresOrMaxAge = Some(Left(DateTime.now.minusDays(1)))),
          Cookie("not-expired", "")
        ), Map.empty)), Map.empty),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("https://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("not-expired"))
  }

  it should "filter hostonly cookies from parent domains" in {
    val jar = new SynchronizedCookieJar(DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(
          Cookie("example.com", "", domain=Some("example.com")),
          Cookie("example.com-hostOnly", "", hostOnly = true, domain=Some("example.com"))
        ), Map.empty)), Map(
          "foo" -> DomainTree(Some(PathTree(List(
            Cookie("foo.example.com", "", domain=Some("foo.example.com")),
            Cookie("foo.example.com-hostOnly", "", hostOnly = true, domain=Some("foo.example.com"))
          ), Map.empty)), Map.empty)
        )),
        "foo" -> DomainTree(Some(PathTree(List(Cookie("foo.com", "")), Map.empty)), Map.empty)
      ))
    )))

    val cookies = jar.get(Uri.fromString("http://foo.example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com", "foo.example.com", "foo.example.com-hostOnly"))
  }

}


