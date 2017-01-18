package plug.cookie

import org.scalatest.{FlatSpec, Matchers}
import plug.Uri

class SynchronizedCookieJarSpec extends FlatSpec with Matchers with TestSuffix {

  "get" should "retrieve all cookies that match path and domain" in {
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

}


