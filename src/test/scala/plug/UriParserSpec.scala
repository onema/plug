package plug

import org.scalatest._

class UriParserSpec extends FlatSpec with Matchers {
  "TryParseScheme" should "extract scheme from string" in {

    // Arrange
    val url = "http://foo/bar"

    // Act
    val r = UriParser.TryParseScheme(url, url.length, 0)

    // Assert
    r should equal(Some(7, "http"))
  }

  "GetChar" should "decode space" in {
    UriParser.GetChar("foo%20bar", 4, 2) should equal(' '.toInt)
  }

  it should "decode colon" in {
    UriParser.GetChar("foo%3Abar", 4, 2) should equal(':'.toInt)
  }

  it should "decode unicode encoded space" in {
    UriParser.GetChar("foo%u0020bar", 5, 4) should equal(' '.toInt)
  }

  it should "decode unicode encoded kanji 常" in {
    UriParser.GetChar("foo%u5e38bar", 5, 4) should equal('常'.toInt)
  }

  "Decode" should "convert %20 in middle of string to space" in {
    UriParser.Decode("foo%20bar") should equal("foo bar")
  }

  it should "convert %20 in at beginning of string to space" in {
    UriParser.Decode("%20bar") should equal(" bar")
  }

  it should "convert %20 in at end of string to space" in {
    UriParser.Decode("foo%20") should equal("foo ")
  }

  "Decode" should "leave %2x unconverted" in {
    UriParser.Decode("foo%2xbar") should equal("foo%2xbar")
  }


  it should "convert %u0020 in middle of string to space" in {
    UriParser.Decode("foo%u0020bar") should equal("foo bar")
  }

  it should "convert %u0020 in at beginning of string to space" in {
    UriParser.Decode("%u0020bar") should equal(" bar")
  }

  it should "convert %u0020 in at end of string to space" in {
    UriParser.Decode("foo%u0020") should equal("foo ")
  }

  it should "leave %u002x unconverted" in {
    UriParser.Decode("foo%u002xbar") should equal("foo%u002xbar")
  }

  it should "leave %u00xx unconverted" in {
    UriParser.Decode("foo%u00xxbar") should equal("foo%u00xxbar")
  }

  it should "leave %u0xxx unconverted" in {
    UriParser.Decode("foo%u0xxxbar") should equal("foo%u0xxxbar")
  }

}


