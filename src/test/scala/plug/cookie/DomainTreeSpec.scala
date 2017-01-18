package plug.cookie

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}



class DomainTreeSpec extends FlatSpec with Matchers with TestSuffix {

  "get" should "fetch cookies from all matching parent domains" in {
    val tree = DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree(List(Cookie("a", "example.com")), Map.empty)), Map(
          "foo" -> DomainTree(Some(PathTree(List(Cookie("a", "foo.example.com")), Map.empty)), Map.empty),
          "bar" -> DomainTree(Some(PathTree(List(Cookie("a", "bar.example.com")), Map.empty)), Map.empty)
        ))
      ))
    ))
    val expected = Set("example.com", "foo.example.com")

    val actual = tree.get(Domain("foo.example.com"), Nil)

    actual.map(_.value).toSet should equal(expected)
  }

  "update" should "not create a node for expired cookie" in {
    val cookie = Cookie("a", "xyz", expires = Some(DateTime.now().minusDays(1)))

    val tree = DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree.empty), Map.empty)
      ))
    ))

    val updated = tree.update(cookie, Domain("foo.example.com"), Nil)

    updated should equal(tree)
  }

  it should "create new nodes for the subdomain" in {
    val cookie = Cookie("a", "xyz")

    val tree = DomainTree(None, Map(
      "com" -> DomainTree(None, Map.empty)
    ))
    val expected = DomainTree(None, Map(
      "com" -> DomainTree(None, Map(
        "example" -> DomainTree(Some(PathTree.empty), Map(
          "foo" -> DomainTree(Some(PathTree(List(cookie), Map.empty)), Map.empty)
        ))
      ))
    ))

    val updated = tree.update(cookie, Domain("foo.example.com"), Nil)

    updated should equal(expected)
  }

  it should "ignore cookies set into a public suffix domain and not create new nodes" in {
    val cookie = Cookie("a", "xyx")

    val tree = DomainTree.emptySuffixTree

    val expected = tree

    val updated = tree.update(cookie, Domain("com.au"), Nil)

    updated should equal(expected)
  }

  it should "ignore cookies set into an existing public suffix domain" in {
    val cookie = Cookie("a", "xyx")

    val tree = DomainTree(None, Map(
      "au" -> DomainTree(None, Map(
        "com" -> DomainTree(Some(PathTree.empty), Map(
          "foo" -> DomainTree.emptyPrefixTree
        ))
      ))
    ))

    val expected = tree

    val updated = tree.update(cookie, Domain("com.au"), Nil)

    updated should equal(expected)
  }
}
