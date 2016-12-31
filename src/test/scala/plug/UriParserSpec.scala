package plug

import org.scalatest._

class UriParserSpec extends FlatSpec with Matchers {
  "tryParseScheme" should "extract scheme from string" in {

    // Arrange
    val url = "http://foo/bar"

    // Act
    val r = UriParser.tryParseScheme(url, url.length, 0)

    // Assert
    r should equal(Some(7, "http"))
  }

  "tryParsePath" should "extract segments from path" in {
    val path = "a/b/c"
    val steps = UriParser.tryParsePath(path,path.length,0,Uri("parse"))
    steps.parts.segments should equal(List("a","b","c"))
    steps.parts.trailingSlash shouldBe false
  }

  it should "extract trailing slash from path" in {
    val path = "a/b/c/"
    val steps = UriParser.tryParsePath(path,path.length,0,Uri("parse"))
    steps.parts.segments should equal(List("a","b","c"))
    steps.parts.trailingSlash shouldBe true
  }

  it should "parse leading '/' as part of first segment" in {
    val path = "/a/b/c"
    val steps = UriParser.tryParsePath(path,path.length,0,Uri("parse"))
    steps.parts.segments should equal(List("/a","b","c"))
  }

  "tryParsePort" should "parse 80" in {
    UriParser.tryParsePort("80", 0, 2) should equal(80)
  }

  it should "parse 80 out of abc80def" in {
    UriParser.tryParsePort("abc80def", 3, 5) should equal(80)
  }

  it should "parse 443 from tail" in {
    UriParser.tryParsePort("https://example.com:443", 20, 23) should equal(443)
  }

  it should "return -1 on port lower than 0" in {
    UriParser.tryParsePort("-100", 0, 2) should equal(-1)
  }

  it should "return -1 on port higher than 65535" in {
    UriParser.tryParsePort("65536", 0, 5) should equal(-1)
  }

  "getChar" should "decode space" in {
    UriParser.getChar("foo%20bar", 4, 2) should equal(' '.toInt)
  }

  it should "decode colon" in {
    UriParser.getChar("foo%3Abar", 4, 2) should equal(':'.toInt)
  }

  it should "decode unicode encoded space" in {
    UriParser.getChar("foo%u0020bar", 5, 4) should equal(' '.toInt)
  }

  it should "decode unicode encoded kanji 常" in {
    UriParser.getChar("foo%u5e38bar", 5, 4) should equal('常'.toInt)
  }

  "decode" should "convert %20 in middle of string to space" in {
    UriParser.decodeString("foo%20bar") should equal("foo bar")
  }

  it should "convert %20 in at beginning of string to space" in {
    UriParser.decodeString("%20bar") should equal(" bar")
  }

  it should "convert %20 in at end of string to space" in {
    UriParser.decodeString("foo%20") should equal("foo ")
  }

  it should "convert several encoded elements in cr%C3%A9ate into single unicode char é" in {
    UriParser.decodeString("cr%C3%A9ate") should equal("créate")
  }

  it should "leave %2x unconverted" in {
    UriParser.decodeString("foo%2xbar") should equal("foo%2xbar")
  }

  it should "convert %u0020 in middle of string to space" in {
    UriParser.decodeString("foo%u0020bar") should equal("foo bar")
  }

  it should "convert %u0020 in at beginning of string to space" in {
    UriParser.decodeString("%u0020bar") should equal(" bar")
  }

  it should "convert %u0020 in at end of string to space" in {
    UriParser.decodeString("foo%u0020") should equal("foo ")
  }

  it should "leave %u002x unconverted" in {
    UriParser.decodeString("foo%u002xbar") should equal("foo%u002xbar")
  }

  it should "leave %u00xx unconverted" in {
    UriParser.decodeString("foo%u00xxbar") should equal("foo%u00xxbar")
  }

  it should "leave %u0xxx unconverted" in {
    UriParser.decodeString("foo%u0xxxbar") should equal("foo%u0xxxbar")
  }

}


