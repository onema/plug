package plug

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/6/17.
  */
class CookieTreeSpec extends FlatSpec with Matchers {

  "get" should "return only cookies for path or parent path" in {
    val tree = CookieTree(List(Cookie("root", "")), Map(
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
    ))

    val cookies = tree.get(List("a", "b", "c"))
    cookies.map(_.name).toSet should equal(Set("root", "a", "a/b", "a/b/c"))
  }

  it should "ignore non match paths" in {
    val tree = CookieTree(List(Cookie("root", "")), Map(
      "x" -> CookieTree(List(Cookie("x", "")), Map.empty),
      "y" -> CookieTree(List(Cookie("y", "")), Map.empty)
    ))

    val cookies = tree.get(List("a", "b", "c"))
    cookies.map(_.name).toSet should equal(Set("root"))
  }

  "update" should "add root cookie to root" in {
    val cookie = Cookie("a", "b")
    val tree = CookieTree(Nil, Map.empty)
    val expected = CookieTree(List(cookie), Map.empty)

    val actual = tree.update(cookie, Nil)

    actual should equal(expected)
  }

  it should "create subtrees for path" in {
    val cookie = Cookie("a", "b")
    val tree = CookieTree(Nil, Map.empty)
    val expected = CookieTree(Nil, Map(
      "a" -> CookieTree(Nil, Map(
        "b" -> CookieTree(Nil, Map(
          "c" -> CookieTree(List(cookie), Map.empty)
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "interweave new subtrees with old" in {
    val cookie = Cookie("a", "b")
    val tree = CookieTree(List(Cookie("root", "")), Map(
      "a" -> CookieTree(List(Cookie("a", "")), Map(
        "b" -> CookieTree(List(Cookie("a/b", "")), Map(
          "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
        )),
        "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
      )),
      "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
    ))
    val expected = CookieTree(List(Cookie("root", "")), Map(
      "a" -> CookieTree(List(Cookie("a", "")), Map(
        "b" -> CookieTree(List(Cookie("a/b", "")), Map(
          "c" -> CookieTree(List(cookie),Map.empty),
          "c1" -> CookieTree(List(Cookie("a/b/c1", "")), Map.empty)
        )),
        "b1" -> CookieTree(List(Cookie("a/b1", "")), Map.empty)
      )),
      "a1" -> CookieTree(List(Cookie("a1", "")), Map.empty)
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "insert cookie into existing list" in {
    val cookie = Cookie("a", "b")
    val tree = CookieTree(Nil, Map(
      "a" -> CookieTree(Nil, Map(
        "b" -> CookieTree(Nil, Map(
          "c" -> CookieTree(List(Cookie("a/b/c","")), Map.empty)
        ))
      ))
    ))
    val expected = CookieTree(Nil, Map(
      "a" -> CookieTree(Nil, Map(
        "b" -> CookieTree(Nil, Map(
          "c" -> CookieTree(List(cookie,Cookie("a/b/c","")), Map.empty)
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "replace existing cookie with same name at same path" in {
    val cookie = Cookie("a", "xyz")
    val tree = CookieTree(List(Cookie("a", "root")), Map(
      "a" -> CookieTree(List(Cookie("a", "a")), Map(
        "b" -> CookieTree(List(Cookie("a", "a/b")), Map(
          "c" -> CookieTree(List(Cookie("a", "a/b/c")), Map(
            "d" -> CookieTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))
    val expected = CookieTree(List(Cookie("a", "root")), Map(
      "a" -> CookieTree(List(Cookie("a", "a")), Map(
        "b" -> CookieTree(List(Cookie("a", "a/b")), Map(
          "c" -> CookieTree(List(Cookie("a", "xyz")), Map(
            "d" -> CookieTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "remove an expired at its path only" in {
    val cookie = Cookie("a", "xyz", expires = Some(DateTime.now().minusDays(1)))
    val tree = CookieTree(List(Cookie("a", "root")), Map(
      "a" -> CookieTree(List(Cookie("a", "a")), Map(
        "b" -> CookieTree(List(Cookie("a", "a/b")), Map(
          "c" -> CookieTree(List(Cookie("a", "a/b/c"),Cookie("aa","a/b/c")), Map(
            "d" -> CookieTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))
    val expected = CookieTree(List(Cookie("a", "root")), Map(
      "a" -> CookieTree(List(Cookie("a", "a")), Map(
        "b" -> CookieTree(List(Cookie("a", "a/b")), Map(
          "c" -> CookieTree(List(Cookie("aa","a/b/c")), Map(
            "d" -> CookieTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }
}
