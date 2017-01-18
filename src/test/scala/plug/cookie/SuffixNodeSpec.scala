package plug.cookie

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by arne on 1/11/17.
  */
class SuffixNodeSpec extends FlatSpec with Matchers with TestSuffix {

  "addSuffix" should "build tree for new suffix" in {
    val expected = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty
      ))
    ))

    val tree = SuffixNode.empty.addSuffix(List("com","example"))

    tree should equal(expected)
  }

  it should "add nodes for a new suffix on an existing tree" in {
    val expected = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty
      )),
      "org" -> SuffixNode.empty
    ))
    val tree = SuffixNode(Map("org" -> SuffixNode.empty)).addSuffix(List("com","example"))

    tree should equal(expected)
  }

  it should "expand tree with new nodes for sub suffix" in {
    val expected = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode(Map(
          "foo" -> SuffixNode.empty
        ))
      ))
    ))
    val existing = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty
      ))
    ))

    val tree = existing.addSuffix(List("com","example","foo"))

    tree should equal(expected)
  }

  it should "branch tree with new nodes for sibling suffix" in {
    val expected = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty,
        "test" -> SuffixNode.empty
      ))
    ))
    val existing = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty
      ))
    ))

    val tree = existing.addSuffix(List("com","test"))

    tree should equal(expected)
  }

  it should "not change tree for existing suffix" in {
    val expected = SuffixNode(Map(
      "com" -> SuffixNode(Map(
        "example" -> SuffixNode.empty
      )),
      "org" -> SuffixNode.empty
    ))

    val tree = expected.addSuffix(List("com","example"))

    tree should equal(expected)
  }

  "splitOnSuffix" should "split example.com on .com" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("com","example"))
    (prefix,suffix) should equal((List("example"),List("com")))
  }

  it should "split foo.example.com on .com" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("com","example","foo"))
    (prefix,suffix) should equal((List("example","foo"),List("com")))
  }

  it should "return an empty prefix for act.edu.au" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("au","edu","act"))
    (prefix,suffix) should equal((Nil,List("au","edu","act")))
  }

  it should "split foo.bar.act.edu.au on .act.edu.au" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("au","edu","act","bar","foo"))
    (prefix,suffix) should equal((List("bar","foo"),List("au","edu","act")))
  }

  it should "split foo.edu.au on .edu.au" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("au","edu","foo"))
    (prefix,suffix) should equal((List("foo"),List("au","edu")))
  }

  it should "return an empty suffix for localhost" in {
    val (prefix,suffix) = suffixes.splitOnSuffix(List("localhost"))
    (prefix,suffix) should equal((List("localhost"),Nil))
  }
}
