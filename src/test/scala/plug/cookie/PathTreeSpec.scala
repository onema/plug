package plug.cookie

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/6/17.
  */
class PathTreeSpec extends FlatSpec with Matchers {

  "get" should "return only cookies for path or parent path" in {
    val tree = PathTree(List(Cookie("root", "")), Map(
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
    ))

    val cookies = tree.get(List("a", "b", "c"))
    cookies.map(_.name).toSet should equal(Set("root", "a", "a/b", "a/b/c"))
  }

  it should "ignore non match paths" in {
    val tree = PathTree(List(Cookie("root", "")), Map(
      "x" -> PathTree(List(Cookie("x", "")), Map.empty),
      "y" -> PathTree(List(Cookie("y", "")), Map.empty)
    ))

    val cookies = tree.get(List("a", "b", "c"))
    cookies.map(_.name).toSet should equal(Set("root"))
  }

  "update" should "add root cookie to root" in {
    val cookie = Cookie("a", "b")
    val tree = PathTree(Nil, Map.empty)
    val expected = PathTree(List(cookie), Map.empty)

    val actual = tree.update(cookie, Nil)

    actual should equal(expected)
  }

  it should "create subtrees for path" in {
    val cookie = Cookie("a", "b")
    val tree = PathTree(Nil, Map.empty)
    val expected = PathTree(Nil, Map(
      "a" -> PathTree(Nil, Map(
        "b" -> PathTree(Nil, Map(
          "c" -> PathTree(List(cookie), Map.empty)
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "interweave new subtrees with old" in {
    val cookie = Cookie("a", "b")
    val tree = PathTree(List(Cookie("root", "")), Map(
      "a" -> PathTree(List(Cookie("a", "")), Map(
        "b" -> PathTree(List(Cookie("a/b", "")), Map(
          "c1" -> PathTree(List(Cookie("a/b/c1", "")), Map.empty)
        )),
        "b1" -> PathTree(List(Cookie("a/b1", "")), Map.empty)
      )),
      "a1" -> PathTree(List(Cookie("a1", "")), Map.empty)
    ))
    val expected = PathTree(List(Cookie("root", "")), Map(
      "a" -> PathTree(List(Cookie("a", "")), Map(
        "b" -> PathTree(List(Cookie("a/b", "")), Map(
          "c" -> PathTree(List(cookie),Map.empty),
          "c1" -> PathTree(List(Cookie("a/b/c1", "")), Map.empty)
        )),
        "b1" -> PathTree(List(Cookie("a/b1", "")), Map.empty)
      )),
      "a1" -> PathTree(List(Cookie("a1", "")), Map.empty)
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "insert cookie into existing list" in {
    val cookie = Cookie("a", "b")
    val tree = PathTree(Nil, Map(
      "a" -> PathTree(Nil, Map(
        "b" -> PathTree(Nil, Map(
          "c" -> PathTree(List(Cookie("a/b/c","")), Map.empty)
        ))
      ))
    ))
    val expected = PathTree(Nil, Map(
      "a" -> PathTree(Nil, Map(
        "b" -> PathTree(Nil, Map(
          "c" -> PathTree(List(cookie,Cookie("a/b/c","")), Map.empty)
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "replace existing cookie with same name at same path" in {
    val cookie = Cookie("a", "xyz")
    val tree = PathTree(List(Cookie("a", "root")), Map(
      "a" -> PathTree(List(Cookie("a", "a")), Map(
        "b" -> PathTree(List(Cookie("a", "a/b")), Map(
          "c" -> PathTree(List(Cookie("a", "a/b/c")), Map(
            "d" -> PathTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))
    val expected = PathTree(List(Cookie("a", "root")), Map(
      "a" -> PathTree(List(Cookie("a", "a")), Map(
        "b" -> PathTree(List(Cookie("a", "a/b")), Map(
          "c" -> PathTree(List(Cookie("a", "xyz")), Map(
            "d" -> PathTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }

  it should "remove an expired at its path only" in {
    val cookie = Cookie("a", "xyz", expires = Some(DateTime.now().minusDays(1)))
    val tree = PathTree(List(Cookie("a", "root")), Map(
      "a" -> PathTree(List(Cookie("a", "a")), Map(
        "b" -> PathTree(List(Cookie("a", "a/b")), Map(
          "c" -> PathTree(List(Cookie("a", "a/b/c"),Cookie("aa","a/b/c")), Map(
            "d" -> PathTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))
    val expected = PathTree(List(Cookie("a", "root")), Map(
      "a" -> PathTree(List(Cookie("a", "a")), Map(
        "b" -> PathTree(List(Cookie("a", "a/b")), Map(
          "c" -> PathTree(List(Cookie("aa","a/b/c")), Map(
            "d" -> PathTree(List(Cookie("a", "a/b/c/d")), Map.empty)
          ))
        ))
      ))
    ))

    val actual = tree.update(cookie,List("a","b","c"))

    actual should equal(expected)
  }
}
