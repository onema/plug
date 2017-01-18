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

  "optionalDomainString" should "return None for empty Domain" in {
    Domain.empty.optionalDomainString should equal(None)
  }

  it should "return domain string" in {
    Domain("foo.example.com").optionalDomainString should equal(Some("foo.example.com"))
  }
}
