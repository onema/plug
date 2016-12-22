package plug

import org.scalatest._

/**
  * Created by arne on 11/22/16.
  */
class UriParserSpec extends FlatSpec with Matchers {

  ///--- Constants ---

  // Escaped version of "Iñtërnâtiônàlizætiøn" (should look similar to "Internationalization" but with extended characteres)
  val INTERNATIONALIZATION = "I\u00f1t\u00ebrn\u00e2ti\u00f4n\u00e0liz\u00e6ti\u00f8n"

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

  "TryParse [scheme]" should "parse https scheme" in assertParse(
    "https://example.com",
    Some(Uri(scheme = "https",
      hostname = Some("example.com"),
      port = Some(443)
    ))
  )

  it should "parse default port from https scheme" in assertParse(
    "https://example.com:443",
    Some(Uri(scheme = "https",
      hostname = Some("example.com"),
      port = Some(443)
    ))
  )

  it should "parse non-default port from https scheme" in assertParse(
    "https://example.com:444",
    Some(Uri(scheme = "https",
      hostname = Some("example.com"),
      port = Some(444),
      usesDefaultPort = false
    ))
  )

  it should "parse ftp scheme and hostname" in assertParse(
    "ftp://example.com",
    Some(Uri(scheme = "ftp",
      hostname = Some("example.com"),
      port = Some(21)
    ))
  )

  it should "parse default port from ftp scheme" in assertParse(
    "https://example.com:21",
    Some(Uri(scheme = "https",
      hostname = Some("example.com"),
      port = Some(21)
    ))
  )

  it should "parse non-default port from ftp scheme" in assertParse(
    "https://example.com:22",
    Some(Uri(scheme = "https",
      hostname = Some("example.com"),
      port = Some(222),
      usesDefaultPort = false
    ))
  )

  it should "parse hostname for unknown scheme without port" in assertParse(
    "unknown://example.com",
    Some(Uri(scheme = "unknown",
      hostname = Some("example.com")
    ))
  )

  it should "parse hostname and explicit port for unknown scheme" in assertParse(
    "unknown://example.com:123",
    Some(Uri(scheme = "unknown",
      hostname = Some("example.com"),
      port = Some(123)
    ))
  )

  it should "ignore case in scheme" in assertParse(
    "hTTp://example.com",
    Some(Uri(scheme = "hTTp",
      hostname = Some("example.com"),
      port = Some(444),
      usesDefaultPort = false
    ))
  )

  it should "fail to parse scheme containing numbers" in assertParse("123://example.com", None)

  it should "fail to parse scheme with a plus sign in it" in assertParse("ht+tp://example.com", None)

  it should "fail to parse scheme with encoding in it" in assertParse("ht%74p://example.com", None)

  it should "fail to parse scheme with invalid character in it" in assertParse("http;//example.com", None)

  it should "fail to parse scheme with colon in it" in assertParse("ht:tp://example.com", None)

  it should "fail to parse uri consisting of just scheme" in assertParse("http", None)

  "TryParse [port]" should "parse http with default port" in assertParse(
    "http://example.com:80",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )

  it should "parse http with non-default port" in assertParse(
    "http://example.com:8888",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(8888),
      usesDefaultPort = false
    ))
  )

  it should "fail to parse uri with invalid port number" in assertParse("http://example.com:65536", None)
  it should "fail to parse uri with invalid char in port" in assertParse("http://example.com:<", None)

  it should "fail to parse uri with user:pass and invalid port number" in assertParse("http://user:pass@example.com:65536", None)
  it should "fail to parse uri with user:pass invalid char in port" in assertParse("http://user:pass@example.com:<", None)

  "TryParse [host]" should "parse simple host name" in assertParse(
    "http://example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )

  it should "parse IPv4 as host name" in assertParse(
    "http://8.8.8.8",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )
  it should "parse IPv4 with default port as host name" in assertParse(
    "http://8.8.8.8:80",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )

  it should "parse IPv4 with non-default port as host name" in assertParse(
    "http://8.8.8.8:8888",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(8888),
      usesDefaultPort = false
    ))
  )
  it should "parse IPv4 with path" in assertParse(
    "http://8.8.8.8/path",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("path")
    ))
  )

  it should "parse IPv4 with query" in assertParse(
    "http://8.8.8.8?a=b",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("a" -> Some("b"))
    ))
  )

  it should "parse IPv6 as hostname" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )
  it should "fail to parse partial IPv6 address" in assertParse("http://[2001:0db8:85a3", None)
  it should "fail to parse IPv6 address followed by invalid char" in assertParse("http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]<", None)

  it should "parse IPv6 with default port" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]:80",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )

  it should "parse IPv6 with non-default port" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]:8888",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(8888),
      usesDefaultPort = false
    ))
  )

  it should "parse IPv6 with default path" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]/path",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("path")
    ))
  )

  it should "parse IPv6 with query" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]?a=b",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("a" -> Some("b"))
    ))
  )

  it should "fail to parse IPv6 with non-hex digits" in assertParse("http://[2001:0db8:85a3:08d3:1319:8a2e:0370:xxxx]", None)

  it should "fail to parse IPv6 with encoding" in assertParse("http://[2001:0db8:85a3:08d3:1319:8a2e:0370:%20]", None)

  it should "fail on hostname with plus sign" in assertParse("http://ex+ample.com", None)

  it should "fail hostname with encoding" in assertParse("http://ex%62mple.com", None)

  it should "parse empty hostname" in assertParse(
    "http://",
    Some(Uri(scheme = "http",
      hostname = Some(""),
      port = Some(80),
      usesDefaultPort = true
    ))
  )
  it should "parse empty hostname with port" in assertParse(
    "http://:8888",
    Some(Uri(scheme = "http",
      hostname = Some(""),
      port = Some(8888),
      usesDefaultPort = true
    ))
  )
  it should "parse empty hostname with path" in assertParse(
    "http:///path",
    Some(Uri(scheme = "http",
      hostname = Some(""),
      port = Some(8888),
      usesDefaultPort = true,
      segments = List("path")
    ))
  )

  // Should this have a trailing slash? And if so, is http://?a=b valid?
  it should "parse empty hostname with query" in assertParse(
    "http:///?a=b",
    Some(Uri(scheme = "http",
      hostname = Some(""),
      port = Some(80),
      usesDefaultPort = true,
      query = List("a" -> Some("b"))
    ))
  )
  it should "parse empty hostname with user & pass" in assertParse(
    "http:///user:pass@",
    Some(Uri(scheme = "http",
      hostname = Some(""),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("user"),
      password = Some("pass")
    ))
  )

  it should "fail on hostname with leading encoding" in assertParse("http://bob:pass@%65xample.com", None)
  it should "fail on hostname with encoding and user/pass" in assertParse("http://bob:pass@ex%62mple.com", None)
  it should "fail on hostname/port with encoding and user/pass" in assertParse("http://bob:pass@ex%62mple.com:80", None)
  it should "fail on hostname with invalid char" in assertParse("http://exam<ple.com", None)
  it should "fail on invalid char in after colon for pass" in assertParse("http://bob:<pass@example.com", None)
  it should "fail on hostname with user/pass and invalid char" in assertParse("http://bob:pass@ex<ample.com", None)
  it should "fail on hostname with encoding and path" in assertParse("http://ex%62mple.com:80/path", None)

  "TryParse [credentials]" should "parse user & pass" in assertParse(
    "http://john.doe:password@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe"),
      password = Some("password")
    ))
  )

  it should "parse user" in assertParse(
    "http://john.doe@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe")
    ))
  )

  it should "parse empty user" in assertParse(
    "http://@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("")
    ))
  )

  it should "parse user & pass from IPv4 uri" in assertParse(
    "http://john.doe:password@8.8.8.8",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe"),
      password = Some("password")
    ))
  )

  it should "parse user from IPv4 uri" in assertParse(
    "http://john.doe@8.8.8.8",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe")
    ))
  )

  it should "parse empty user from IPv4 uri" in assertParse(
    "http://@8.8.8.8",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("")
    ))
  )

  it should "parse user & pass from IPv6 uri" in assertParse(
    "http://john.doe:password@[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe"),
      password = Some("password")
    ))
  )

  it should "parse user from IPv6 uri" in assertParse(
    "http://john.doe@[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe")
    ))
  )

  it should "parse empty user from IPv6 uri" in assertParse(
    "http://@[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("")
    ))
  )

  it should "parse user with plus sign and plain password" in assertParse(
    "http://john+doe:password@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john doe"),
      password = Some("password")
    ))
  )

  it should "parse user with encoded colon and plain password" in assertParse(
    "http://john%3Adoe:password@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john:doe"),
      password = Some("password")
    ))
  )

  it should "parse user and password with encoded colon " in assertParse(
    "http://john.doe:pass%3Aword@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe"),
      password = Some("pass:word")
    ))
  )

  it should "parse user and password with plus sign " in assertParse(
    "http://john.doe:pass+word@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john.doe"),
      password = Some("pass+word")
    ))
  )

  it should "parse user with plus and password with encoded space " in assertParse(
    "http://john+doe:pass%20word@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john doe"),
      password = Some("pass word")
    ))
  )

  it should "parse user and passsword each with encoded colon" in assertParse(
    "http://john%3Adoe:pass%3Aword@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some("john:doe"),
      password = Some("pass:word")
    ))
  )

  it should "parse user with leading encoding" in assertParse(
    "http://%3Adoe@example.com",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      user = Some(":doe")))
  )

  it should "fail to parse url with null in password" in assertParse("http://user:\0pass@example.com", None)

  "TryParse [path]" should "track that hostname had a trailing slash" in assertParse(
    "http://example.com/",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      trailingSlash = true
    ))
  )

  it should "track that hostname with port had a trailing slash" in assertParse(
    "http://example.com:80/",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      trailingSlash = true
    ))
  )

  it should "track that IPv4 hostname had a trailing slash" in assertParse(
    "http://8.8.8.8/",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      trailingSlash = true
    ))
  )

  it should "track that IPv6 hostname had a trailing slash" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]/",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      trailingSlash = true
    ))
  )

  it should "not count double-slash as trailing slash" in assertParse(
    "http://example.com//",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("/")
    ))
  )

  it should "parse path with multiple double-slash segments" in assertParse(
    "http://example.com//foo//bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("/foo", "/bar")
    ))
  )

  it should "parse hostname and single segment" in assertParse(
    "http://example.com/path",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("path")
    ))
  )

  it should "parse hostname and single segment and trailing slash" in assertParse(
    "http://example.com/path/",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("path"),
      trailingSlash = true
    ))
  )

  it should "parse hostname and single segment and trailing double-slash" in assertParse(
    "http://example.com/path//",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("path", "/")
    ))
  )

  it should "parse multi-segment with encoding" in assertParse(
    "http://example.com/abc/foo%20bar/xyz",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("abc", "foo%20bar", "xyz")
    ))
  )

  it should "parse single segment with caret" in assertParse(
    "http://example.com/foo^bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo^bar")
    ))
  )

  it should "parse single segment with vertical bar" in assertParse(
    "http://example.com/foo|bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo|bar")
    ))
  )

  it should "parse single segment with square brackets" in assertParse(
    "http://example.com/[foobar]",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("[foobar]")
    ))
  )

  it should "parse single segment with curly brackets" in assertParse(
    "http://example.com/{foobar}",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("{foobar}")
    ))
  )

  it should "parse segment with semi-colon" in assertParse(
    "http://www.ietf.org/rfc;15/rfc2396.txt",
    Some(Uri(scheme = "http",
      hostname = Some("www.ietf.org"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("rfc;15", "rfc2396.txt")
    ))
  )

  it should "parse segment with trailing semi-colon" in assertParse(
    "http://www.ietf.org/rfc;15/rfc2396.txt;",
    Some(Uri(scheme = "http",
      hostname = Some("www.ietf.org"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("rfc;15", "rfc2396.txt;")
    ))
  )

  it should "parse segment with semi-colon at start" in assertParse(
    "http://www.ietf.org/;15/rfc2396.txt",
    Some(Uri(scheme = "http",
      hostname = Some("www.ietf.org"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List(";15", "rfc2396.txt")
    ))
  )

  it should "parse utf-8 segment un-decoded" in assertParse(
    "http://example.com/cr%c3%a9ate",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("cr%c3%a9ate")
    ))
  )

  it should "treat backslash after hostname as trailing slash " in assertParse(
    "http://example.com\\",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List(),
      trailingSlash = true
    )),
    Some("http://example.com/")
  )

  it should "treat backslash as path separator" in assertParse(
    "http://example.com/foo\\bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo", "bar")
    )),
    Some("http://example.com/foo/bar")
  )

  it should "convert backslash at start of segment as slash" in assertParse(
    "http://example.com/foo/\\bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo", "/bar")
    )),
    Some("http://example.com/foo//bar")
  )

  it should "treat trailing back-slash in path as trailing slash" in assertParse(
    "http://example.com/foo/bar\\",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo", "bar"),
      trailingSlash = true
    )),
    Some("http://example.com/foo/bar/")
  )

  it should "treat back-slash after hostname/ as double-slash" in assertParse(
    "http://example.com/\\foo/bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("/foo", "bar")
    )),
    Some("http://example.com//foo/bar")
  )

  it should "treat back-slash after hostname as slash ???" in assertParse(
    "http://example.com\\foo/bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo", "bar")
    )),
    Some("http://example.com/foo/bar")
  )

  it should "fail to parse path with invalid char" in assertParse("http://example.com/path<foo", None)

  "TryParse [query]" should "parse trailing slash and empty query" in assertParse(
    "http://example.com/?",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List(),
      trailingSlash = true
    ))
  )

  it should "parse empty query with slash after hostname" in assertParse(
    "http://example.com?",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true
    ))
  )

  it should "parse and decode utf-8 query value" in assertParse(
    "http://example.com/?key=cr%C3%A9ate",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("key" -> Some("cr\u00e9ate"))
    ))
  )

  it should "parse encoded query key without value" in assertParse(
    "http://example.com?x+y",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x y" -> None)
    ))
  )

  it should "parse query key with double-percent and no value" in assertParse(
    "http://example.com?x%%y",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x%%y" -> None)
    )),
    Some("http://example.com?x%25%25y")
  )

  it should "parse query key with single percent and no value" in assertParse(
    "http://example.com?x%",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x%" -> None)
    )),
    Some("http://example.com?x%25")
  )

  it should "parse query key with single percent followed by a single char and no value" in assertParse(
    "http://example.com?x%y",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x%y" -> null)
    )),
    Some("http://example.com?x%25y")
  )

  it should "parse query key with single percent followed by two chars and no value" in assertParse(
    "http://example.com?x%yz",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x%yz" -> None)
    )),
    Some("http://example.com?x%25yz")
  )

  it should "parse unicode query key and no value" in assertParse(
    "http://example.com?x%u0020y",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x y" -> None)
    )),
    Some("http://example.com?x+y")
  )

  it should "parse invalidly unicode encoded query key and no value" in assertParse(
    "http://example.com?x%uxabcy",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x%uxabcy" -> None)
    )),
    Some("http://example.com?x%25uxabcy")
  )

  it should "parse encoded query key and value" in assertParse(
    "http://example.com?x+y=a+b",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("x y" -> Some("a b"))
    ))
  )

  it should "parse query with leading ampersand" in assertParse(
    "http://example.com?&a=b",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("" -> None, "a" -> Some("b"))
    ))
  )

  it should "parse query with trailing ampersand" in assertParse(
    "http://example.com?a=b&",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("a" -> Some("b"))
    )),
    Some("http://example.com?a=b")
  )

  it should "parse query with double ampersand" in assertParse(
    "http://example.com?a&&b",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      query = List("a" -> None, "" -> None, "b" -> None)
    ))
  )

  it should "fail on query with invalid char" in assertParse("http://example.com?x<y", None)

  "TryParse [fragment]" should "parse fragment" in assertParse(
    "http://example.com#fragment",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("fragment")
    ))
  )

  it should "parse fragment on IPv4 hostname" in assertParse(
    "http://8.8.8.8#fragment",
    Some(Uri(scheme = "http",
      hostname = Some("8.8.8.8"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("fragment")
    ))
  )

  it should "parse fragment on IPv6 hostname" in assertParse(
    "http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]#fragment",
    Some(Uri(scheme = "http",
      hostname = Some("[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("fragment")
    ))
  )

  it should "parse empty fragment" in assertParse(
    "http://example.com#",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("")
    ))
  )

  it should "parse fragment with plus sign" in assertParse(
    "http://example.com#a+b",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("a b")
    ))
  )

  it should "parse fragment in square brackets" in assertParse(
    "http://example.com#[a]",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("[a]")
    )),
    Some("http://example.com#%5Ba%5D")
  )

  it should "parse encoded fragment" in assertParse(
    "http://example.com#%1a%1A%20",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      fragment = Some("\\u001a\\u001A\\u0020")
    )),
    Some("http://example.com#%1A%1A+")
  )

  it should "fail on fragment with invalid char" in assertParse("http://example.com#x<y", None)

  "TryParse [misc]" should "fail on null string" in assertParse(null, None)

  it should "fail empty string" in assertParse("", None)

  it should "parse user/pass/host/port and encoded segments and double slash in path and mixed query params" in assertParse(
    "http://user:password@example.com:81/path/foo%20bar/path//@blah?ready&set=&go=foo/bar",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(81),
      user = Some("user"),
      password = Some("password"),
      usesDefaultPort = false,
      segments = List("path", "foo%20bar", "path", "/@blah"),
      query = List("ready" -> None, "set" -> Some(""), "go" -> Some("foo/bar"))
    ))
  )

  it should "parse user/pass/host/port and encoded segments and double slash in path and fragment" in assertParse(
    "http://user:password@example.com:81/path/foo%20bar/path//@blah#yo",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(81),
      user = Some("user"),
      password = Some("password"),
      usesDefaultPort = false,
      segments = List("path", "foo%20bar", "path", "/@blah"),
      fragment = Some("yo")
    ))
  )

  it should "parse user/pass/host/port and encoded segments and double slash in path, mixed query and fragment" in assertParse(
    "http://user:password@example.com:81/path/foo%20bar/path//@blah/?ready&set=&go=foo/bar#yo",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(81),
      user = Some("user"),
      password = Some("password"),
      usesDefaultPort = false,
      trailingSlash = true,
      segments = List("path", "foo%20bar", "path", "/@blah"),
      query = List("ready" -> None, "set" -> Some(""), "go" -> Some("foo/bar")),
      fragment = Some("yo")
    ))
  )

  it should "parse ftp hostname with path" in assertParse(
    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    Some(Uri(scheme = "ftp",
      hostname = Some("ftp.is.co.za"),
      port = Some(21),
      usesDefaultPort = true,
      segments = List("rfc", "rfc1808.txt")
    ))
  )

  it should "parse ftp with weird hostname and empty fragment" in assertParse(
    "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm#",
    Some(Uri(scheme = "ftp",
      hostname = Some("10.0.0.1"),
      port = Some(21),
      user = Some("cnn.example.com&story=breaking_news"),
      usesDefaultPort = true,
      segments = List("top_story.htm"),
      fragment = Some("")
    ))
  )

  it should "parse special characters in path and query and fragment" in assertParse(
    "http://host/seg^ment?qu^ery=a|b^c#fo|o#b^ar",
    Some(Uri(scheme = "http",
      hostname = Some("host"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("seg^ment"),
      query = List("qu^ery" -> Some("a|b^c")),
      fragment = Some("fo|o#b^ar")
    ))
  )

  it should "parse square brackets in query key" in assertParse(
    "http://example.com/foo?bar[123]=abc",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo"),
      query = List("bar[123]" -> Some("abc"))
    )),
    Some("http://example.com/foo?bar%5B123%5D=abc")
  )

  it should "parse curly brackets in query value" in assertParse(
    "http://example.com/foo?bar={xyz}",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo"),
      query = List("bar" -> Some("{xyz}"))
    )),
    Some("http://example.com/foo?bar=%7Bxyz%7D")
  )

  it should "parse curly brackets in fragment" in assertParse(
    "http://example.com/foo#{xyz}",
    Some(Uri(scheme = "http",
      hostname = Some("example.com"),
      port = Some(80),
      usesDefaultPort = true,
      segments = List("foo"),
      fragment = Some("{xyz}")
    ))
  )

  def assertParse(uri: String, parts: Option[Uri], out: Option[String] = None) = {
    withClue(s"Uri: $uri") {
      UriParser.TryParse(uri) match {
        case None => parts match {
          case None =>
          case Some(_) => fail(s"Expected parse of '$uri' to succeed")
        }
        case Some(parsed) =>
          parts match {
            case None => fail(s"Did not expect parse to succeed")
            case Some(parts2) =>
              parsed should equal(parts2)
              //parsed.toString should equal(out.getOrElse(uri))
          }
      }
    }
  }
}
