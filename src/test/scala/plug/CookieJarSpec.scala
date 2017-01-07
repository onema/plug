package plug

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/7/17.
  */
class CookieJarSpec extends FlatSpec with Matchers {

  "get" should "retrieve all cookies that match path and domain" in {
    val jar = new CookieJar(Map(
      "example.com" -> CookieTree(List(Cookie("example.com", "")), Map(
        "a" -> CookieTree(List(Cookie("a", "")), Map(
          "b" -> CookieTree(List(Cookie("a/b", "")), Map(
            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
            )),
            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
          )),
          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
        )),
        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
      )),
      "foo.com" -> CookieTree(List(Cookie("foo.com","")),Map.empty)
    ))

    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com","a","a/b","a/b/c"))
  }

  it should "filter secure cookies for http" in {
    val jar = new CookieJar(Map(
      "example.com" -> CookieTree(List(Cookie("example.com", "", secure = true)), Map(
        "a" -> CookieTree(List(Cookie("a", "")), Map(
          "b" -> CookieTree(List(Cookie("a/b", "",secure = true)), Map(
            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
            )),
            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
          )),
          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
        )),
        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
      )),
      "foo.com" -> CookieTree(List(Cookie("foo.com","")),Map.empty)
    ))

    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("a","a/b/c"))
  }

  it should "not filter secure cookies for https" in {
    val jar = new CookieJar(Map(
      "example.com" -> CookieTree(List(Cookie("example.com", "", secure = true)), Map(
        "a" -> CookieTree(List(Cookie("a", "")), Map(
          "b" -> CookieTree(List(Cookie("a/b", "",secure = true)), Map(
            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
            )),
            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
          )),
          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
        )),
        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
      )),
      "foo.com" -> CookieTree(List(Cookie("foo.com","")),Map.empty)
    ))

    val cookies = jar.get(Uri.fromString("https://example.com/a/b/c").get)
    cookies.map(_.name).toSet should equal(Set("example.com","a","a/b","a/b/c"))
  }
}
