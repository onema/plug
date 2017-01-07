package plug

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/6/17.
  */
class CookieTreeSpec extends FlatSpec with Matchers {

  "get" should "return only cookies for path or parent path" in {
    val tree = CookieTree(List(Cookie("root","")),Map(
      "a" -> CookieTree(List(Cookie("a","")),Map(
        "b" -> CookieTree(List(Cookie("a/b","")), Map(
          "c" -> CookieTree(List(Cookie("a/b/c","")),Map(
            "d" -> CookieTree(List(Cookie("a/b/c/d","")),Map.empty)
          )),
          "c1" -> CookieTree(List(Cookie("a/b/c1","")),Map.empty)
        )),
        "b1" -> CookieTree(List(Cookie("a/b1","")),Map.empty)
      )),
      "a1" -> CookieTree(List(Cookie("a1","")),Map.empty)
    ))

    val cookies = tree.get(List("a","b","c"))
    cookies.map(_.name).toSet should equal(Set("root","a","a/b","a/b/c"))
  }

  it should "ignore non match paths" in {
    val tree = CookieTree(List(Cookie("root","")),Map(
      "x" -> CookieTree(List(Cookie("x","")),Map.empty),
    "y" -> CookieTree(List(Cookie("y","")),Map.empty)
    ))

    val cookies = tree.get(List("a","b","c"))
    cookies.map(_.name).toSet should equal(Set("root"))

  }
}
