package plug.cookie

import org.scalatest.{FlatSpec, Matchers}

class PathSpec extends FlatSpec with Matchers {

  "apply" should "parse '/' into root Path" in {
    Path("/") should equal(Path(Nil))
  }

  it should "parse /foo/bar into parts" in {
    Path("/foo/bar") should equal(Path(List("foo","bar")))
  }

  it should "return empty for empty path" in {
    Path("") should equal(Path.empty)
  }

  it should "return empty for None" in {
    Path(None) should equal(Path.empty)
  }

  it should "canonicalize to case insensitive form" in {
    Path("/FOO/bar/Baz") should equal(Path(List("foo","BAR","baz")))
  }

  "isEmpty" should "be false for root Path" in {
    Path(Nil).isEmpty shouldBe false
  }

  "asOptionalString" should "round trip path" in {
    Path("/foo/bar").asOptionalString should equal(Some("/foo/bar"))
  }

  it should "return '/' on Path with no segments" in {
    Path(Nil).asOptionalString should equal(Some("/"))
  }

  it should "return lower cased form" in {
    Path("/FOO/bar/Baz").asOptionalString should equal(Some("/foo/bar/baz"))
  }

  it should "return None for"
  "isSubPathOf" should "be true if origin path is a sub path" in {
    Path(List("foo","bar","baz")).isSubPathOf(Path(List("foo","bar"))) shouldBe true
  }

  it should "be true if origin path is the same path" in {
    Path(List("foo","bar")).isSubPathOf(Path(List("foo","bar"))) shouldBe true
  }

  it should "be false if origin path is a parent path" in {
    Path(List("foo","bar")).isSubPathOf(Path(List("foo","bar","baz"))) shouldBe false
  }

  it should "be false if provided path is different path" in {
      Path(List("baz")).isSubPathOf(Path(List("foo","bar"))) shouldBe false
  }

  it should "be false if provided path is empty" in {
    Path(List("baz")).isSubPathOf(Path.empty) shouldBe false
  }

  it should "be false if origin path is empty" in {
    Path.empty.isSubPathOf(Path(List("baz"))) shouldBe false
  }

}
