package plug.cookie

import org.scalatest.{FlatSpec, Matchers}

class DomainSpec extends FlatSpec with Matchers {

  "apply" should "split domain string into reversed parts" in {
    Domain("foo.bar.com") should equal(Domain(List("com", "bar", "foo")))
  }

  it should "not split an IP address" in {
    Domain("10.10.1.1") should equal(Domain(List("10.10.1.1")))
  }

  it should "strip leading dot" in {
    Domain(".example.com") should equal(Domain(List("com","example")))
  }

  it should "convert None to empty" in {
    Domain(None) should equal(Domain.empty)
  }

  it should "convert empty domain string to empty" in {
    Domain("") should equal(Domain.empty)
  }

  it should "canonicalize to case insensitive form" in {
    Domain("Foo.eXample.COM") should equal(Domain("foo.EXAMPLE.com"))
  }

  "asOptionalString" should "return None for empty Domain" in {
    Domain.empty.asOptionalString should equal(None)
  }

  it should "return domain string" in {
    Domain("foo.example.com").asOptionalString should equal(Some("foo.example.com"))
  }

  it should "return lower cased form" in {
    Domain("Foo.eXample.COM").asOptionalString should equal(Some("foo.example.com"))
  }

  "isSubDomainOf" should "be true if origin domain is a sub domain" in {
    Domain("foo.example.com").isSubDomainOf(Domain("example.com")) shouldBe true
  }

  it should "be true if origin domain is the same domain" in {
    Domain("example.com").isSubDomainOf(Domain("example.com")) shouldBe true
  }

  it should "be false if orgin domain is a different domain" in {
    Domain("example.com").isSubDomainOf(Domain("example.org")) shouldBe false
  }

  it should "be false if provided domain is empty" in {
    Domain("example.com").isSubDomainOf(Domain.empty) shouldBe false
  }

  it should "be false if origin domain is empty" in {
    Domain.empty.isSubDomainOf(Domain("foo.example.com")) shouldBe false
  }
}
