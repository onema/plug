package plug

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/10/17.
  */
class DomainTreeSpec extends FlatSpec with Matchers {

  Map(
    "com" -> DomainTree(None, Map(
      "example" -> DomainTree(Some(CookieTree(Nil, Map.empty)), Map(
        "foo" -> DomainTree(Some(CookieTree(Nil, Map.empty)), Map.empty),
        "bar" -> DomainTree(Some(CookieTree(Nil, Map.empty)), Map.empty)
      ))
    )),
    "10.10.0.1" -> DomainTree(Some(CookieTree(Nil, Map.empty)), Map.empty)
  )

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

    val actual = tree.get(List("com","example","foo"),Nil)

    actual.map(_.value).toSet should equal(expected)
  }

}
