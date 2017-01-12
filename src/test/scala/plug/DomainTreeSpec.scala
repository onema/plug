package plug

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/10/17.
  */
class DomainTreeSpec extends FlatSpec with Matchers {

  "get" should "fetch cookies from all matching parent domains" in {
    val tree = DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(CookieTree(List(Cookie("a", "example.com")), Map.empty)), Map(
          "foo" -> DomainTree(Some(CookieTree(List(Cookie("a", "foo.example.com")), Map.empty)), Map.empty),
          "bar" -> DomainTree(Some(CookieTree(List(Cookie("a", "bar.example.com")), Map.empty)), Map.empty)
        ))
      ))
    ))
    val expected = Set("example.com", "foo.example.com")

    val actual = tree.get(List("com", "example", "foo"), Nil)

    actual.map(_.value).toSet should equal(expected)
  }

  "update" should "not create a node for expired cookie" in {
    val cookie = Cookie("a", "xyz", expires = Some(DateTime.now().minusDays(1)))

    val tree = DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(CookieTree(Nil, Map.empty)), Map.empty)
      ))
    ))

    val updated = tree.update(cookie, List("foo", "example", "com"), Nil)

    updated should equal(tree)
  }

}
