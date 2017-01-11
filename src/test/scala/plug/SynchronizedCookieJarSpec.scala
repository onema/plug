package plug

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/7/17.
  */
class SynchronizedCookieJarSpec extends FlatSpec with Matchers {

//  "get" should "retrieve all cookies that match path and domain" in {
//    val jar = new SynchronizedCookieJar(Map(
//      "example.com" -> CookieTree(List(Cookie("example.com", "")), Map(
//        "a" -> CookieTree(List(Cookie("a", "")), Map(
//          "b" -> CookieTree(List(Cookie("a/b", "")), Map(
//            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
//              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
//            )),
//            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
//          )),
//          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
//        )),
//        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
//      )),
//      "foo.com" -> CookieTree(List(Cookie("foo.com", "")), Map.empty)
//    ))
//
//    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
//    cookies.map(_.name).toSet should equal(Set("example.com", "a", "a/b", "a/b/c"))
//  }
//
//  it should "filter secure cookies for http" in {
//    val jar = new SynchronizedCookieJar(Map(
//      "example.com" -> CookieTree(List(Cookie("example.com", "", secure = true)), Map(
//        "a" -> CookieTree(List(Cookie("a", "")), Map(
//          "b" -> CookieTree(List(Cookie("a/b", "", secure = true)), Map(
//            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
//              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
//            )),
//            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
//          )),
//          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
//        )),
//        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
//      )),
//      "foo.com" -> CookieTree(List(Cookie("foo.com", "")), Map.empty)
//    ))
//
//    val cookies = jar.get(Uri.fromString("http://example.com/a/b/c").get)
//    cookies.map(_.name).toSet should equal(Set("a", "a/b/c"))
//  }
//
//  it should "not filter secure cookies for https" in {
//    val jar = new SynchronizedCookieJar(Map(
//      "example.com" -> CookieTree(List(Cookie("example.com", "", secure = true)), Map(
//        "a" -> CookieTree(List(Cookie("a", "")), Map(
//          "b" -> CookieTree(List(Cookie("a/b", "", secure = true)), Map(
//            "c" -> CookieTree(List(Cookie("a/b/c", "")), Map(
//              "d" -> CookieTree(List(Cookie("a/b/c/d", "")), Map.empty)
//            )),
//            "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
//          )),
//          "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
//        )),
//        "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
//      )),
//      "foo.com" -> CookieTree(List(Cookie("foo.com", "")), Map.empty)
//    ))
//
//    val cookies = jar.get(Uri.fromString("https://example.com/a/b/c").get)
//    cookies.map(_.name).toSet should equal(Set("example.com", "a", "a/b", "a/b/c"))
//  }
//
//  "update" should "add uri to cookie, if it has no uri" in {
//    val cookie = Cookie("a", "b")
//    val uri = Uri.fromString("http://example.com/").get
//    val jar = new SynchronizedCookieJar(Map.empty)
//    val expected = Map(
//      "example.com" -> CookieTree(List(Cookie("a", "b", Some(uri))), Map.empty)
//    )
//
//    jar.update(cookie, uri)
//    val actual = jar.toDomainTrees
//
//    actual should equal(expected)
//  }
//
//  it should "add domain to cookie if it has none" in {
//    val cookie = Cookie("a", "b", Uri.fromString("http:///foo"))
//    val uri = Uri.fromString("http://example.com/foo/bar").get
//    val jar = new SynchronizedCookieJar(Map.empty)
//    val expected = Map(
//      "example.com" -> CookieTree(Nil,Map(
//        "foo" -> CookieTree(List(Cookie("a", "b", Uri.fromString("http://example.com/foo"))), Map.empty)
//      ))
//    )
//
//    jar.update(cookie, uri)
//    val actual = jar.toDomainTrees
//
//    actual should equal(expected)
//  }
//
//  it should "add path to cookie if it has none" in {
//    val cookie = Cookie("a", "b", Uri.fromString("http://example.com"))
//    val uri = Uri.fromString("http://example.com/foo").get
//    val jar = new SynchronizedCookieJar(Map.empty)
//    val expected = Map(
//      "example.com" -> CookieTree(Nil,Map(
//        "foo" -> CookieTree(List(Cookie("a", "b", Uri.fromString("http://example.com/foo"))), Map.empty)
//      ))
//    )
//
//    jar.update(cookie, uri)
//    val actual = jar.toDomainTrees
//
//    actual should equal(expected)
//  }
//
//  it should "ignore a cookie with the wrong domain" in {
//    fail("not implemented")
//  }
//
//  it should "ignore a cookie with a path that's more specific than the uri path" in {
//    fail("not implemented")
//  }
//
//  it should "ignore case in domain" in {
//    fail("not implemented")
//  }
//
//  it should "not ignore case in path" in {
//    fail("not implemented")
//  }
//
//  it should "update the domain/path specific tree using the cookie path" in {
//    fail("not implemented")
//  }
}
