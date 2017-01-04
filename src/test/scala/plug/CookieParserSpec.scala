package plug

import org.scalatest.{FlatSpec, Matchers}

class CookieParserSpec extends FlatSpec with Matchers {

  "parseCookieHeader" should "extract single cookie from header starting with $Version" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE", "/acme"))
  }

  it should "extract single cookie from header starting with $Version followed by a comma" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\", Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE", "/acme"))
  }

  it should "extract single cookie without $Version" in {
    val cookies = CookieParser.parseCookieHeader("Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE", "/acme"))
  }

  it should "extract comma separated cookies" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\", Shipping=\"FedEx\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE", "/acme"),
      C("Part_Number", "Rocket_Launcher_0001", "/acme"),
      C("Shipping", "FedEx", "/acme")
    )
  }

  it should "extract comma separated cookies with leading $Version" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE", "/acme"),
      C("Part_Number", "Rocket_Launcher_0001", "/acme")
    )
  }

  it should "extrace cookie with unquoted value and quotes in value" in {
    val cookies = CookieParser.parseCookieHeader("Customer=WILE_E_COYOTE; $Path=\"/acme\", Part_Number=\"Rocket \\\"Launcher\\\" 0001\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE", "/acme"),
      C("Part_Number", "Rocket \"Launcher\" 0001", "/acme")
    )
  }

  it should "roundtrip header created with Cookie.renderCookieHeader" in {
    val cookie = Cookie("Customer", "WILE_E_COYOTE", Uri.fromString("http://localhost/acme"),setCookie=false)
    val cookies = CookieParser.parseCookieHeader(cookie.toCookieHeader())
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE", "/acme"))
  }

  it should "parse bad cookie names" in {
    val cookies = CookieParser.parseCookieHeader("foo=\"bar\"; [index.php]scayt_verLang=5; authtoken=\"1234\"")
    assertCookies(cookies,
      C("foo", "bar"),
      C("[index.php]scayt_verLang", "5"), // check original test what the name actually parses as
      C("authtoken", "1234")
    )
  }
  it should "parse bad cookie names 2" in {
    val cookies = CookieParser.parseCookieHeader("  foo=\"bar\"; lithiumLogin:successfactors=~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP; authtoken=\"1234\"  ")
    assertCookies(cookies,
      C("foo", "bar"), // No path... need to deal with this in C
      C("[index.php]scayt_verLang", "5"), // check original test what the name actually parses as
      C("authtoken", "1234")
    )
  }

  //  public void ParseBadCookie2() {
  //    var cookies = DreamCookie.ParseCookieHeader("  foo=\"bar\"; lithiumLogin:successfactors=~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP; authtoken=\"1234\"  ");
  //    Assert.AreEqual(3, cookies.Count, "Failed to parse cookies, wrong number of resulting cookies");
  //    Assert.AreEqual(cookies[0].Name, "foo", "bad cookie name");
  //    Assert.AreEqual(cookies[0].Value, "bar", "bad cookie value");
  //    Assert.AreEqual(cookies[2].Name, "authtoken", "bad cookie name");
  //    Assert.AreEqual(cookies[2].Value, "1234", "bad cookie value");
  //  }
  //
  //    [Test]
  //  public void ParseBadCookie3() {
  //    var cookies = DreamCookie.ParseCookieHeader("  foo=\"bar\", lithiumLogin:successfactors=~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP; authtoken=\"1234\"  ");
  //    Assert.AreEqual(3, cookies.Count, "Failed to parse cookies, wrong number of resulting cookies");
  //    Assert.AreEqual(cookies[0].Name, "foo", "bad cookie name");
  //    Assert.AreEqual(cookies[0].Value, "bar", "bad cookie value");
  //    Assert.AreEqual(cookies[2].Name, "authtoken", "bad cookie name");
  //    Assert.AreEqual(cookies[2].Value, "1234", "bad cookie value");
  //  }
  //
  //    [Test]
  //  public void ParseBadCookie4() {
  //    var cookies = DreamCookie.ParseCookieHeader("  foo=\"bar\"; hel,lo=\"wo,~rld\"; authtoken=\"1234\"  ");
  //    Assert.AreEqual(4, cookies.Count, "Failed to parse cookies, wrong number of resulting cookies");
  //    Assert.AreEqual(cookies[0].Name, "foo", "bad cookie name");
  //    Assert.AreEqual(cookies[0].Value, "bar", "bad cookie value");
  //    Assert.AreEqual(cookies[1].Name, "hel", "bad cookie name");
  //    Assert.IsNull(cookies[1].Value, null, "bad cookie value");
  //    Assert.AreEqual(cookies[2].Name, "lo", "bad cookie name");
  //    Assert.AreEqual(cookies[2].Value, "wo,~rld", "bad cookie value");
  //    Assert.AreEqual(cookies[3].Name, "authtoken", "bad cookie name");
  //    Assert.AreEqual(cookies[3].Value, "1234", "bad cookie value");
  //  }
  //
  //    [Test]
  //  public void ParseBadCookie5() {
  //    var cookies = DreamCookie.ParseCookieHeader("  foo=\"bar\", hello=wo;;rld; authtoken=\"1234\"  ");
  //    Assert.AreEqual(4, cookies.Count, "Failed to parse cookies, wrong number of resulting cookies");
  //    Assert.AreEqual(cookies[0].Name, "foo", "bad cookie name");
  //    Assert.AreEqual(cookies[0].Value, "bar", "bad cookie value");
  //    Assert.AreEqual(cookies[3].Name, "authtoken", "bad cookie name");
  //    Assert.AreEqual(cookies[3].Value, "1234", "bad cookie value");
  //  }
  //
  //    [Test]
  //  public void ParseCookies_with_unquoted_values() {
  //    List<DreamCookie> result = DreamCookie.ParseCookieHeader("__utma=134392366.697651776.1256325927.1256943466.1256946079.27; __utmz=134392366.1256946079.27.2.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:SteveB/Bugs; LOOPFUSE=78fe6a69-de6f-494f-9cf1-7e4fbe7a1c38; __kti=1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktv=9f88-8fb4-514a-a2cb1247bd5bce8; _mkto_trk=id:954-WGP-507&token:_mch-mindtouch.com-1256705011527-41439; __utma=249966356.478917817.1256718580.1256718580.1256946891.2; __utmz=249966356.1256946891.2.2.utmcsr=bugs.developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/view.php; __utmc=134392366; __utmb=134392366.7.10.1256946079; PHPSESSID=bed8f2d85712b33f1a3804856045b374; __utmb=249966356.1.10.1256946891; __utmc=249966356; __kts=1256946891198,http%3A%2F%2Fcampaign.mindtouch.com%2FEvents%2FSharepoint,http%3A%2F%2Fbugs.developer.mindtouch.com%2Fview.php%3Fid%3D7255; __ktt=c2e3-a511-ce1b-438b124a7df79be,abc,");
  //    Assert.AreEqual(15, result.Count);
  //    Assert.AreEqual("__utma", result[0].Name);
  //    Assert.AreEqual("134392366.697651776.1256325927.1256943466.1256946079.27", result[0].Value);
  //    Assert.AreEqual("__utmz", result[1].Name);
  //    Assert.AreEqual("134392366.1256946079.27.2.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:SteveB/Bugs", result[1].Value);
  //    Assert.AreEqual("LOOPFUSE", result[2].Name);
  //    Assert.AreEqual("78fe6a69-de6f-494f-9cf1-7e4fbe7a1c38", result[2].Value);
  //    Assert.AreEqual("__kti", result[3].Name);
  //    Assert.AreEqual("1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,", result[3].Value);
  //    Assert.AreEqual("__ktv", result[4].Name);
  //    Assert.AreEqual("9f88-8fb4-514a-a2cb1247bd5bce8", result[4].Value);
  //    Assert.AreEqual("_mkto_trk", result[5].Name);
  //    Assert.AreEqual("id:954-WGP-507&token:_mch-mindtouch.com-1256705011527-41439", result[5].Value);
  //    Assert.AreEqual("__utma", result[6].Name);
  //    Assert.AreEqual("249966356.478917817.1256718580.1256718580.1256946891.2", result[6].Value);
  //    Assert.AreEqual("__utmz", result[7].Name);
  //    Assert.AreEqual("249966356.1256946891.2.2.utmcsr=bugs.developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/view.php", result[7].Value);
  //    Assert.AreEqual("__utmc", result[8].Name);
  //    Assert.AreEqual("134392366", result[8].Value);
  //    Assert.AreEqual("__utmb", result[9].Name);
  //    Assert.AreEqual("134392366.7.10.1256946079", result[9].Value);
  //    Assert.AreEqual("PHPSESSID", result[10].Name);
  //    Assert.AreEqual("bed8f2d85712b33f1a3804856045b374", result[10].Value);
  //    Assert.AreEqual("__utmb", result[11].Name);
  //    Assert.AreEqual("249966356.1.10.1256946891", result[11].Value);
  //    Assert.AreEqual("__utmc", result[12].Name);
  //    Assert.AreEqual("249966356", result[12].Value);
  //    Assert.AreEqual("__kts", result[13].Name);
  //    Assert.AreEqual("1256946891198,http%3A%2F%2Fcampaign.mindtouch.com%2FEvents%2FSharepoint,http%3A%2F%2Fbugs.developer.mindtouch.com%2Fview.php%3Fid%3D7255", result[13].Value);
  //    Assert.AreEqual("__ktt", result[14].Name);
  //    Assert.AreEqual("c2e3-a511-ce1b-438b124a7df79be,abc,", result[14].Value);
  //  }
  //
  //    [Test]
  //  public void ParseCookies_with_unquoted_values2() {
  //    List<DreamCookie> result = DreamCookie.ParseCookieHeader("__utma=134392366.2030730348.1275932450.1276553042.1276556836.19; __utmz=134392366.1276207881.9.3.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:arnec/bugs; _mkto_trk=id:954-WGP-507&token:_mch-mindtouch.com-1270756717014-83706; WRUID=0; __kti=1274382964652,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktv=2f4-f02d-634b-51e2128b724d7c2; __qca=P0-2102347259-1274460371553; PHPSESSID=307e779182909ab37932b4dffe77c40a; __utmc=134392366; __kts=1274382964673,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktt=631f-d0a2-648e-e0b128b724d7c2; authtoken=\"1_634121336269193470_4254e33b49bc1ee0a72c5716200e296b\"; __utmb=134392366.6.10.1276556836");
  //    Assert.AreEqual(13, result.Count);
  //    Assert.AreEqual("__utma", result[0].Name);
  //    Assert.AreEqual("134392366.2030730348.1275932450.1276553042.1276556836.19", result[0].Value);
  //    Assert.AreEqual("__utmz", result[1].Name);
  //    Assert.AreEqual("134392366.1276207881.9.3.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:arnec/bugs", result[1].Value);
  //    Assert.AreEqual("_mkto_trk", result[2].Name);
  //    Assert.AreEqual("id:954-WGP-507&token:_mch-mindtouch.com-1270756717014-83706", result[2].Value);
  //    Assert.AreEqual("WRUID", result[3].Name);
  //    Assert.AreEqual("0", result[3].Value);
  //    Assert.AreEqual("__kti", result[4].Name);
  //    Assert.AreEqual("1274382964652,http%3A%2F%2Fwww.mindtouch.com%2F,", result[4].Value);
  //    Assert.AreEqual("__ktv", result[5].Name);
  //    Assert.AreEqual("2f4-f02d-634b-51e2128b724d7c2", result[5].Value);
  //    Assert.AreEqual("__qca", result[6].Name);
  //    Assert.AreEqual("P0-2102347259-1274460371553", result[6].Value);
  //    Assert.AreEqual("PHPSESSID", result[7].Name);
  //    Assert.AreEqual("307e779182909ab37932b4dffe77c40a", result[7].Value);
  //    Assert.AreEqual("__utmc", result[8].Name);
  //    Assert.AreEqual("134392366", result[8].Value);
  //    Assert.AreEqual("__kts", result[9].Name);
  //    Assert.AreEqual("1274382964673,http%3A%2F%2Fwww.mindtouch.com%2F,", result[9].Value);
  //    Assert.AreEqual("__ktt", result[10].Name);
  //    Assert.AreEqual("631f-d0a2-648e-e0b128b724d7c2", result[10].Value);
  //    Assert.AreEqual("authtoken", result[11].Name);
  //    Assert.AreEqual("1_634121336269193470_4254e33b49bc1ee0a72c5716200e296b", result[11].Value);
  //    Assert.AreEqual("__utmb", result[12].Name);
  //    Assert.AreEqual("134392366.6.10.1276556836", result[12].Value);
  //  }
  //
  //    [Test]
  //  public void Parse_SetCookie() {
  //    List<DreamCookie> result = DreamCookie.ParseSetCookieHeader("Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\"");
  //    Assert.AreEqual(2, result.Count);
  //    Assert.AreEqual("Customer", result[0].Name);
  //    Assert.AreEqual("WILE_E_COYOTE", result[0].Value);
  //    Assert.AreEqual(1, result[0].Version);
  //    Assert.AreEqual("/acme", result[0].Path);
  //    Assert.AreEqual(false, result[0].HttpOnly);
  //    Assert.AreEqual("Part_Number", result[1].Name);
  //    Assert.AreEqual("Rocket_Launcher_0001", result[1].Value);
  //    Assert.AreEqual(1, result[1].Version);
  //    Assert.AreEqual("/acme", result[1].Path);
  //    Assert.AreEqual(false, result[1].HttpOnly);
  //  }
  //
  //    [Test]
  //  public void Parse_SetCookie_with_HttpOnly() {
  //    List<DreamCookie> result = DreamCookie.ParseSetCookieHeader("Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\"; HttpOnly, Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\"; HttpOnly");
  //    Assert.AreEqual(2, result.Count);
  //    Assert.AreEqual("Customer", result[0].Name);
  //    Assert.AreEqual("WILE_E_COYOTE", result[0].Value);
  //    Assert.AreEqual(1, result[0].Version);
  //    Assert.AreEqual(true, result[0].HttpOnly);
  //    Assert.AreEqual("/acme", result[0].Path);
  //    Assert.AreEqual("Part_Number", result[1].Name);
  //    Assert.AreEqual("Rocket_Launcher_0001", result[1].Value);
  //    Assert.AreEqual(1, result[1].Version);
  //    Assert.AreEqual("/acme", result[1].Path);
  //    Assert.AreEqual(true, result[1].HttpOnly);
  //  }
  //
  //    [Test]
  //  public void Parse_5_cookies_separated_by_semicolon() {
  //    List<DreamCookie> cookies = DreamCookie.ParseCookieHeader("authtoken=\"3_633644459231333750_74b1192b1846f065523d01ac18c772c5\"; PHPSESSID=34c4b18a50a91dd99adb1ed1e6b570cb; __utma=14492279.2835659202033839600.1228849092.1228849092.1228849092.1; __utmc=14492279; __utmz=14492279.1228849092.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)");
  //    Assert.AreEqual(5, cookies.Count);
  //    Assert.AreEqual("authtoken", cookies[0].Name);
  //    Assert.AreEqual("3_633644459231333750_74b1192b1846f065523d01ac18c772c5", cookies[0].Value);
  //    Assert.AreEqual("PHPSESSID", cookies[1].Name);
  //    Assert.AreEqual("34c4b18a50a91dd99adb1ed1e6b570cb", cookies[1].Value);
  //  }
  //
  //    [Test]
  //  public void Parse_2_cookies_separated_by_semicolon() {
  //    List<DreamCookie> cookies = DreamCookie.ParseCookieHeader("PHPSESSID=663e17bc2eaef4e355c6e6fe1bb86c04; authtoken=1_633644446772281250_c3dd88ad4539197ef12f3614e91fec8f");
  //    Assert.AreEqual(2, cookies.Count);
  //    Assert.AreEqual("PHPSESSID", cookies[0].Name);
  //    Assert.AreEqual("663e17bc2eaef4e355c6e6fe1bb86c04", cookies[0].Value);
  //    Assert.AreEqual("authtoken", cookies[1].Name);
  //    Assert.AreEqual("1_633644446772281250_c3dd88ad4539197ef12f3614e91fec8f", cookies[1].Value);
  //  }
  //
  //    [Test]
  //  public void Parse_cookie_without_path_or_domain() {
  //    List<DreamCookie> cookies = DreamCookie.ParseCookieHeader("foo=\"bar\"");
  //    Assert.IsNull(cookies[0].Path);
  //    Assert.IsNull(cookies[0].Domain);
  //  }
  //
  //    [Test]
  //  public void Parse_cookie_sample_from_wikipedia() {
  //    List<DreamCookie> cookies = DreamCookie.ParseSetCookieHeader("RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net; HttpOnly");
  //    Assert.AreEqual(1, cookies.Count);
  //    Assert.AreEqual("RMID", cookies[0].Name);
  //    Assert.AreEqual("732423sdfs73242", cookies[0].Value);
  //    DateTime expires = DateTimeUtil.ParseExactInvariant("Fri, 31-Dec-2010 23:59:59 GMT", "ddd, dd-MMM-yyyy HH:mm:ss 'GMT'");
  //    Assert.AreEqual(expires, cookies[0].Expires);
  //    Assert.AreEqual("/", cookies[0].Path);
  //
  //    // TODO (steveb): it seems wrong that we check for 'example.net' instead of '.example.net'
  //    Assert.AreEqual("example.net", cookies[0].Domain);
  //    Assert.AreEqual(true, cookies[0].HttpOnly);
  //  }

  object C {
    def apply(name: String, value: String, path: String): C = C(name,value,Some(path))
    def apply(name: String, value: String): C = C(name,value,None)
  }

  case class C(name: String, value: String, path: Option[String] = None)

  def assertName(cookie: Cookie, name: String) = withClue("Bad Cookie Name:") {
    cookie.name should equal(name)
  }

  def assertValue(cookie: Cookie, value: String) = withClue("Bad Cookie Value:") {
    cookie.value should equal(value)
  }

  def assertPath(cookie: Cookie, path: Option[String]) = withClue("Bad Cookie Path:") {
    cookie.path should equal(path)
  }

  def assertCookie(cookie: Cookie, test: C) = {
    assertName(cookie, test.name)
    assertValue(cookie, test.value)
    assertPath(cookie, test.path)
  }

  def assertCookies(cookies: List[Cookie], testCookies: C*) = {
    withClue("Wrong number of cookies parsed:") {
      cookies.length should equal(testCookies.length)
    }
    cookies.zip(testCookies).zipWithIndex.foreach {
      case ((cookie, test), idx) => withClue(s"Cookie $idx:") {
        assertCookie(cookie, test)
      }
    }
  }

  def assertSingleCookie(cookies: List[Cookie], test: C) = {
    withClue("Wrong number of cookies parsed:") {
      cookies.length should equal(1)
    }
    assertCookie(cookies.head, test)
  }

}
