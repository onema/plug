package plug

import org.scalatest._

/**
  * Created by arne on 12/24/16.
  */
class UriSpec extends FlatSpec with Matchers {

  val baseUri = Uri.fromString("http://example.com").get

  "encode" should "convert char é to several encoded elements %C3%A9" in {
    Uri.encode("créate", UriEncoding.Default) should equal("cr%C3%A9ate")
  }

  it should "convert kanji 常 to several encoded elements" in {
    Uri.encode("常", UriEncoding.Default) should equal("%E5%B8%B8")
  }

  "encodeUserInfo" should "replace space with +" in {
    Uri.encodeUserInfo("foo bar") should equal("foo+bar")
  }

  it should "replace : with %3A" in {
    Uri.encodeUserInfo("foo:bar") should equal("foo%3Abar")
  }

  "parseParamsAsPairs" should "return Nil on empty string" in {
    Uri.parseParamsAsPairs("") should equal(Nil)
  }

  it should "parse leading & as empty-string key with no value" in {
    Uri.parseParamsAsPairs("&foo=bar") should equal(List("" -> None, "foo" -> Some("bar")))
  }

  it should "parse trailing & as empty-string key with no value" in {
    Uri.parseParamsAsPairs("foo=bar&") should equal(List("foo" -> Some("bar"), "" -> None))
  }

  it should "parse double & as empty-string key with no value" in {
    Uri.parseParamsAsPairs("baz=fop&&foo=bar") should equal(List("baz" -> Some("fop"), "" -> None, "foo" -> Some("bar")))
  }

  it should "parse pair without = key with no value" in {
    Uri.parseParamsAsPairs("foo") should equal(List("foo" -> None))
  }

  it should "parse name=value as pair" in {
    Uri.parseParamsAsPairs("foo=bar") should equal(List("foo" -> Some("bar")))
  }

  it should "decode key" in {
    Uri.parseParamsAsPairs("foo%20bar=baz") should equal(List("foo bar" -> Some("baz")))
  }

  it should "decode value" in {
    Uri.parseParamsAsPairs("foo=baz%20bar") should equal(List("foo" -> Some("baz bar")))
  }

  "toUriString" should "replace password when requested" in {
    Uri.fromString("http://foo:bar@baz.com").get.toUriString(false) should equal("http://foo:xxx@baz.com")
  }

  "path" should "contain trailing slash if trailingSlash is set" in {
    Uri("http", "example.com", segments = List("a", "bb", "ccc"), trailingSlash = true).path should equal("/a/bb/ccc/")
  }

  it should "not contain trailing slash if trailingSlash is not set" in {
    Uri("http", "example.com", segments = List("a", "bb", "ccc")).path should equal("/a/bb/ccc")
  }

  "query" should "return none if params are None" in {
    Uri("http", "example.com").query should equal(None)
  }

  it should "return empty string if params are empty" in {
    Uri("http", "example.com", params = Some(Nil)).query should equal(Some(""))
  }

  it should "render params" in {
    Uri("http", "example.com", params = Some(List("foo" -> None, "" -> None, "bar" -> Some("baz")))).query should equal(
      Some("foo&&bar=baz")
    )
  }

  "userInfo" should "not encode user" in {
    Uri.fromString("http://foo+bar:baz@example.com").get.userInfo should equal(Some("foo bar:baz"))
  }

  it should "not encode password" in {
    Uri.fromString("http://foo:bar+baz@example.com").get.userInfo should equal(Some("foo:bar baz"))
  }

  it should "have a leading colon if user is not defined" in {
    baseUri.withCredentials(None, Some("baz")).userInfo should equal(Some(":baz"))
  }

  it should "not have a trailing colon if password is not defined" in {
    baseUri.withCredentials(Some("foo"), None).userInfo should equal(Some("foo"))
  }

  it should "be None if no user or password are defined" in {
    baseUri.userInfo should equal(None)
  }

  "withParam(String,String)" should "add a single param to uri without any" in {
    baseUri.withParam("foo", "bar").params should equal(Some(List("foo" -> Some("bar"))))
  }

  it should "append param to uri with existing params" in {
    baseUri.withParam("a", "b").withParam("c", "d").params should equal(
      Some(List("a" -> Some("b"), "c" -> Some("d")))
    )
  }

  it should "ignore duplicate params" in {
    baseUri.withParam("a", "b").withParam("a", "b").params should equal(
      Some(List("a" -> Some("b"), "a" -> Some("b")))
    )
  }

  "withParam(String,Option[String])" should "allow adding of value-less parameter" in {
    baseUri.withParam("a", None).params should equal(Some(List("a" -> None)))
  }

  "withParam(String)" should "add value-less parameter" in {
    baseUri.withParam("a").params should equal(Some(List("a" -> None)))
  }

  "withParams" should "create new params list on Uri without params" in {
    baseUri.withParams(List("a" -> Some("b"), "c" -> Some("d"))).params should equal(
      Some(List("a" -> Some("b"), "c" -> Some("d")))
    )
  }

  it should "append new params at end of existing list" in {
    baseUri.withParam("x", "y").withParams(List("a" -> Some("b"), "c" -> Some("d"))).params should equal(
      Some(List("x" -> Some("y"), "a" -> Some("b"), "c" -> Some("d")))
    )
  }

  "withParamsFrom" should "add parms from one Uri to another" in {
    val other = Uri.fromString("http://foo.com?a=b&c=d").get
    baseUri.withParam("x", "y").withParamsFrom(other).params should equal(
      Some(List("x" -> Some("y"), "a" -> Some("b"), "c" -> Some("d")))
    )
  }

  "withQuery" should "parse and add parameters from querystring" in {
    baseUri.withParam("x", "y").withQuery("a=b&c=d").params should equal(
      Some(List("x" -> Some("y"), "a" -> Some("b"), "c" -> Some("d")))
    )
  }

  "withoutQuery" should "remove all query parameters" in {
    Uri.fromString("http://foo.com?a=b&c=d").get.withoutQuery().params should equal(None)
  }

  "withoutParams" should "remove all query parameters" in {
    Uri.fromString("http://foo.com?a=b&c=d").get.withoutQuery().params should equal(None)
  }

  "withoutParams(String)" should "remove all occurences of key from existing params list" in {
    baseUri.copy(params=Some(List("a"->Some("b"),"c"->Some("d"),"a"->Some("b2"),"e"->Some("f"))))
      .withoutParams("a").params should equal(Some(List("c"->Some("d"),"e"->Some("f"))))
  }

  "withCredentials" should "add credenials" in {
    val uri = baseUri.withCredentials(Some("user"), Some("password"))
    uri.user should equal(Some("user"))
    uri.password should equal(Some("password"))
  }

  it should "replace existing credenials" in {
    val uri = baseUri
      .withCredentials(Some("foo"), Some("bar"))
      .withCredentials(Some("user"), Some("password"))
    uri.user should equal(Some("user"))
    uri.password should equal(Some("password"))
  }

  it should "remove user when provided None" in {
    val uri = baseUri
      .withCredentials(Some("user"), Some("password"))
      .withCredentials(None, Some("password"))
    uri.user should equal(None)
    uri.password should equal(Some("password"))
  }

  it should "remove password when provided None" in {
    val uri = baseUri
      .withCredentials(Some("user"), Some("password"))
      .withCredentials(Some("user"), None)
    uri.user should equal(Some("user"))
    uri.password should equal(None)
  }

  "withCredentialsFrom" should "copy credentials from other uri" in {
    val other = baseUri.withCredentials(Some("foo"),Some("bar"))
    val uri = baseUri.withCredentialsFrom(other)
    uri.user should equal(Some("foo"))
    uri.password should equal(Some("bar"))
  }

  "withoutCredentials" should "strip credentials from uri" in {
    val uri = baseUri.withCredentials(Some("user"), Some("password")).withoutCredentials()
    uri.user should equal(None)
    uri.password should equal(None)
  }

  "withFragment(String)" should "add fragment to uri" in {
    baseUri.withFragment("foo").fragment should equal(Some("foo"))
  }

  it should "replace existing fragment on uri" in {
    baseUri.withFragment("foo").withFragment("bar").fragment should equal(Some("bar"))
  }

  it should "create trailing '#' when provided empty string" in {
    baseUri.withFragment("").toUriString() should equal(baseUri.toUriString+"#")
  }

  "withFragment(Option[String])" should "remove fragment if provided None" in {
    baseUri.withFragment("foo").withFragment(None).fragment should equal(None)
  }

  "withoutFragment" should "remove fragment" in {
    baseUri.withFragment("foo").withoutFragment().fragment should equal(None)
  }

  "withFirstSegments" should "only contain leading path elements" in {
    baseUri.copy(segments = List("a","b","c")).withFirstSegments(2).segments should equal(
      List("a","b")
    )
  }

  it should "remove all segments when provided 0" in {
    baseUri.copy(segments = List("a","b","c")).withFirstSegments(0).segments should equal(Nil)
  }

  it should "leave path unchanged if provided number greater than segment count" in {
    baseUri.copy(segments = List("a","b","c")).withFirstSegments(5).segments should equal(
      List("a","b","c")
    )
  }

  "wihoutFirstSegments" should "trim 'prefix' from path" in {
    baseUri.copy(segments = List("a","b","c")).withoutFirstSegments(1).segments should equal(
      List("b","c")
    )
  }

  it should "leave path unchanged if provided 0" in {
    baseUri.copy(segments = List("a","b","c")).withoutFirstSegments(0).segments should equal(
      List("a","b","c")
    )
  }

  it should "strip path if provided number greater than segment count" in {
    baseUri.copy(segments = List("a","b","c")).withoutFirstSegments(5).segments should equal(Nil)
  }

  "withoutLastSegment" should "remove last segment" in {
    baseUri.copy(segments = List("a","b","c")).withoutLastSegment().segments should equal(
      List("a","b")
    )
  }

  "withoutLastSegments" should "remove trailing segments" in {
    baseUri.copy(segments = List("a","b","c")).withoutLastSegments(2).segments should equal(
      List("a")
    )
  }

  it should "strip path if provided number larger than segments" in {
    baseUri.copy(segments = List("a","b","c")).withoutLastSegments(5).segments should equal(Nil)
  }

  "withoutPath" should "strip path from uri" in {
    baseUri.copy(segments = List("a","b","c")).withoutPath().segments should equal(Nil)
  }

  "withoutPathQueryFragment" should "strip all past host:port" in {
    Uri.fromString("http://example.com/a/b/c?foo=bar#baz").get
      .withoutPathQueryFragment() should equal(baseUri)
  }

  "withoutCredentialsPathQueryFragment" should "strip all except host:port" in {
    Uri.fromString("http://user:pass@example.com/a?foo=bar#baz").get
      .withoutCredentialsPathQueryFragment() should equal(baseUri)
  }

  "withTrailingSlash" should "add trailing slash to uri without path" in {
    Uri.fromString("http://example.com").get.withTrailingSlash().toUriString should equal("http://example.com/")
  }

  it should "add trailing slash to uri with path" in {
    Uri.fromString("http://example.com/a/b").get.withTrailingSlash().toUriString should equal("http://example.com/a/b/")
  }


  "withoutTrailingSlash" should "remove trailing slash from uri without path" in {
    Uri.fromString("http://example.com/").get.withoutTrailingSlash().toUriString should equal("http://example.com")
  }

  it should "remove trailing slash from uri with path" in {
    Uri.fromString("http://example.com/a/b/").get.withoutTrailingSlash().toUriString should equal("http://example.com/a/b")
  }

  "withScheme" should "replace Uri scheme" in {
    baseUri.withScheme("https").toUriString should equal("https://example.com")
  }

  "withHost" should "replace host in Uri" in {
    Uri.fromString("http://u:p@example.com/a/b?foo=bar").get
      .withHost("a.org").toUriString should equal("http://u:p@a.org/a/b?foo=bar")
  }

  "withPort" should "replace port in Uri" in {
    Uri.fromString("http://example.com:881").get.withPort(882).toUriString should equal("http://example.com:882")
  }

  it should "unset usesDefaultPort when new port is non-default" in {
    val uri = baseUri.withPort(882)
    uri.port should equal(882)
    uri.usesDefaultPort shouldBe false
  }

  it should "set usesDefaultPort when new port is default" in {
    val uri = baseUri.copy(port=882)
    uri.usesDefaultPort shouldBe false
    val uri2 = uri.withPort(80)
    uri2.port should equal(80)
    uri2.usesDefaultPort shouldBe true
  }
}
