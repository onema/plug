package plug.cookie

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{FlatSpec, Matchers}

class CookieParserSpec extends FlatSpec with Matchers {

  "parseCookieHeader" should "extract single cookie from header starting with $Version" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE"))
  }

  it should "extract single cookie from header starting with $Version followed by a comma" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\", Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE"))
  }

  it should "extract single cookie without $Version" in {
    val cookies = CookieParser.parseCookieHeader("Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"")
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE"))
  }

  it should "extract comma separated cookies" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\", Shipping=\"FedEx\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE"),
      C("Part_Number", "Rocket_Launcher_0001"),
      C("Shipping", "FedEx")
    )
  }

  it should "extract comma separated cookies with leading $Version" in {
    val cookies = CookieParser.parseCookieHeader("$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE"),
      C("Part_Number", "Rocket_Launcher_0001")
    )
  }

  it should "extrace cookie with unquoted value and quotes in value" in {
    val cookies = CookieParser.parseCookieHeader("Customer=WILE_E_COYOTE; $Path=\"/acme\", Part_Number=\"Rocket \\\"Launcher\\\" 0001\"; $Path=\"/acme\"")
    assertCookies(cookies,
      C("Customer", "WILE_E_COYOTE"),
      C("Part_Number", "Rocket \"Launcher\" 0001")
    )
  }

  it should "roundtrip header created with Cookie.renderCookieHeader" in {
    val cookie = Cookie("Customer", "WILE_E_COYOTE")
    val cookies = CookieParser.parseCookieHeader(cookie.toCookieHeader)
    assertSingleCookie(cookies, C("Customer", "WILE_E_COYOTE"))
  }

  it should "parse all cookies even when one has a bad cookie name" in {
    val cookies = CookieParser.parseCookieHeader("foo=\"bar\"; [index.php]scayt_verLang=5; authtoken=\"1234\"")
    assertCookies(cookies,
      C("foo", "bar"),
      C("[index.php]scayt_verLang", "5"), // check original test what the name actually parses as
      C("authtoken", "1234")
    )
  }

  it should "parse all cookies even when one has a bad cookie name 2" in {
    val cookies = CookieParser.parseCookieHeader("  foo=\"bar\"; lithiumLogin:successfactors=~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP; authtoken=\"1234\"  ")
    assertCookies(cookies,
      C("foo", "bar"),
      C("lithiumLogin:successfactors", "~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP"), // check original test what the name actually parses as
      C("authtoken", "1234")
    )
  }

  it should "parse all cookies even when one has a bad cookie name and some are comma separated" in {
    val cookies = CookieParser.parseCookieHeader("  foo=\"bar\", lithiumLogin:successfactors=~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP; authtoken=\"1234\"  ")
    assertCookies(cookies,
      C("foo", "bar"),
      C("lithiumLogin:successfactors", "~2acHBr09HxytcqIXV~eVqhSr8s74VfDTjhQ8XU615EaYeGn-7OdDSN70BshVnsYG71yPbJvKPoZzHl05KP"), // check original test what the name actually parses as
      C("authtoken", "1234")
    )
  }

  it should "parse all cookies even when one has a comma in the cookie name" in {
    val cookies = CookieParser.parseCookieHeader("  foo=\"bar\"; hel,lo=\"wo,~rld\"; authtoken=\"1234\"  ")
    assertCookies(cookies,
      C("foo", "bar"),
      C("hel", null),
      C("lo", "wo,~rld"),
      C("authtoken", "1234")
    )
  }

  it should "parse all cookies even when one has semi-colons in its unquoted value" in {
    val cookies = CookieParser.parseCookieHeader("  foo=\"bar\", hello=wo;;rld; authtoken=\"1234\"  ")
    assertCookies(cookies,
      C("foo", "bar"),
      C("hello", "wo"),
      C("rld", null),
      C("authtoken", "1234")
    )
  }

  it should "parse value with comma in it when pairs are semicolon separated" in {
    val cookies = CookieParser.parseCookieHeader("foo=bar; __kti=1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,;")
    assertCookies(cookies,
      C("foo", "bar"),
      C("__kti", "1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,")
    )
  }

  it should "parse value with comma in it when pairs are semicolon separated 2" in {
    val cookies = CookieParser.parseCookieHeader("foo=bar; __kti=1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,")
    assertCookies(cookies,
      C("foo", "bar"),
      C("__kti", "1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,")
    )
  }

  it should "parse value with comma in it when pairs are semicolon separated 3" in {
    val cookies = CookieParser.parseCookieHeader("foo=bar; __kti=1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,; bar=foo;")
    assertCookies(cookies,
      C("foo", "bar"),
      C("__kti", "1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,"),
      C("bar", "foo")
    )
  }

  it should "parse cookies with unquoted values" in {
    val cookies = CookieParser.parseCookieHeader("__utma=134392366.697651776.1256325927.1256943466.1256946079.27; __utmz=134392366.1256946079.27.2.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:SteveB/Bugs; LOOPFUSE=78fe6a69-de6f-494f-9cf1-7e4fbe7a1c38; __kti=1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktv=9f88-8fb4-514a-a2cb1247bd5bce8; _mkto_trk=id:954-WGP-507&token:_mch-mindtouch.com-1256705011527-41439; __utma=249966356.478917817.1256718580.1256718580.1256946891.2; __utmz=249966356.1256946891.2.2.utmcsr=bugs.developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/view.php; __utmc=134392366; __utmb=134392366.7.10.1256946079; PHPSESSID=bed8f2d85712b33f1a3804856045b374; __utmb=249966356.1.10.1256946891; __utmc=249966356; __kts=1256946891198,http%3A%2F%2Fcampaign.mindtouch.com%2FEvents%2FSharepoint,http%3A%2F%2Fbugs.developer.mindtouch.com%2Fview.php%3Fid%3D7255; __ktt=c2e3-a511-ce1b-438b124a7df79be,abc,")
    assertCookies(cookies,
      C("__utma", "134392366.697651776.1256325927.1256943466.1256946079.27"),
      C("__utmz", "134392366.1256946079.27.2.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:SteveB/Bugs"),
      C("LOOPFUSE", "78fe6a69-de6f-494f-9cf1-7e4fbe7a1c38"),
      C("__kti", "1256208055528,http%3A%2F%2Fwww.mindtouch.com%2F,"),
      C("__ktv", "9f88-8fb4-514a-a2cb1247bd5bce8"),
      C("_mkto_trk", "id:954-WGP-507&token:_mch-mindtouch.com-1256705011527-41439"),
      C("__utma", "249966356.478917817.1256718580.1256718580.1256946891.2"),
      C("__utmz", "249966356.1256946891.2.2.utmcsr=bugs.developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/view.php"),
      C("__utmc", "134392366"),
      C("__utmb", "134392366.7.10.1256946079"),
      C("PHPSESSID", "bed8f2d85712b33f1a3804856045b374"),
      C("__utmb", "249966356.1.10.1256946891"),
      C("__utmc", "249966356"),
      C("__kts", "1256946891198,http%3A%2F%2Fcampaign.mindtouch.com%2FEvents%2FSharepoint,http%3A%2F%2Fbugs.developer.mindtouch.com%2Fview.php%3Fid%3D7255"),
      C("__ktt", "c2e3-a511-ce1b-438b124a7df79be,abc,")
    )
  }

  it should "parse cookies with unquoted values 2 " in {
    val cookies = CookieParser.parseCookieHeader("__utma=134392366.2030730348.1275932450.1276553042.1276556836.19; __utmz=134392366.1276207881.9.3.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:arnec/bugs; _mkto_trk=id:954-WGP-507&token:_mch-mindtouch.com-1270756717014-83706; WRUID=0; __kti=1274382964652,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktv=2f4-f02d-634b-51e2128b724d7c2; __qca=P0-2102347259-1274460371553; PHPSESSID=307e779182909ab37932b4dffe77c40a; __utmc=134392366; __kts=1274382964673,http%3A%2F%2Fwww.mindtouch.com%2F,; __ktt=631f-d0a2-648e-e0b128b724d7c2; authtoken=\"1_634121336269193470_4254e33b49bc1ee0a72c5716200e296b\"; __utmb=134392366.6.10.1276556836")
    assertCookies(cookies,
      C("__utma", "134392366.2030730348.1275932450.1276553042.1276556836.19"),
      C("__utmz", "134392366.1276207881.9.3.utmcsr=developer.mindtouch.com|utmccn=(referral)|utmcmd=referral|utmcct=/User:arnec/bugs"),
      C("_mkto_trk", "id:954-WGP-507&token:_mch-mindtouch.com-1270756717014-83706"),
      C("WRUID", "0"),
      C("__kti", "1274382964652,http%3A%2F%2Fwww.mindtouch.com%2F,"),
      C("__ktv", "2f4-f02d-634b-51e2128b724d7c2"),
      C("__qca", "P0-2102347259-1274460371553"),
      C("PHPSESSID", "307e779182909ab37932b4dffe77c40a"),
      C("__utmc", "134392366"),
      C("__kts", "1274382964673,http%3A%2F%2Fwww.mindtouch.com%2F,"),
      C("__ktt", "631f-d0a2-648e-e0b128b724d7c2"),
      C("authtoken", "1_634121336269193470_4254e33b49bc1ee0a72c5716200e296b"),
      C("__utmb", "134392366.6.10.1276556836")
    )
  }

  it should "parse 5 cookies separated by semicolon" in {
    val cookies = CookieParser.parseCookieHeader("authtoken=\"3_633644459231333750_74b1192b1846f065523d01ac18c772c5\"; PHPSESSID=34c4b18a50a91dd99adb1ed1e6b570cb; __utma=14492279.2835659202033839600.1228849092.1228849092.1228849092.1; __utmc=14492279; __utmz=14492279.1228849092.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)")
    assertCookies(cookies,
      C("authtoken", "3_633644459231333750_74b1192b1846f065523d01ac18c772c5"),
      C("PHPSESSID", "34c4b18a50a91dd99adb1ed1e6b570cb"),
      C("__utma", "14492279.2835659202033839600.1228849092.1228849092.1228849092.1"),
      C("__utmc", "14492279"),
      C("__utmz", "14492279.1228849092.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)")
    )
  }

  it should "parse 2 cookies separated by a semicolon" in {
    val cookies = CookieParser.parseCookieHeader("PHPSESSID=663e17bc2eaef4e355c6e6fe1bb86c04; authtoken=1_633644446772281250_c3dd88ad4539197ef12f3614e91fec8f")
    assertCookies(cookies,
      C("PHPSESSID", "663e17bc2eaef4e355c6e6fe1bb86c04"),
      C("authtoken", "1_633644446772281250_c3dd88ad4539197ef12f3614e91fec8f")
    )
  }

  it should "parse a cookie without path or domain" in {
    val cookies = CookieParser.parseCookieHeader("foo=\"bar\"")
    assertCookies(cookies,
      C("foo", "bar")
    )
  }

  "parseSetCookieHeader" should "parse set cookies" in {
    val cookies = CookieParser.parseSetCookieHeader("Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\", Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\"")
    assertSetCookies(cookies,
      CS("Customer","WILE_E_COYOTE",path=Some("/acme"),version=1),
      CS("Part_Number","Rocket_Launcher_0001",path=Some("/acme"),version=1)
    )
  }

  it should "parse set cookies with httpOnly" in {
    val cookies = CookieParser.parseSetCookieHeader("Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\"; HttpOnly, Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\"; HttpOnly")
    assertSetCookies(cookies,
      CS("Customer","WILE_E_COYOTE",path=Some("/acme"),version=1,httpOnly = true),
      CS("Part_Number","Rocket_Launcher_0001",path=Some("/acme"),version=1, httpOnly = true)
    )
  }

  it should "parse a sample set cookie from wikipedia" in {
    val cookies = CookieParser.parseSetCookieHeader("RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net; HttpOnly")
    assertSetCookies(cookies,
      CS("RMID","732423sdfs73242",
        domain=Some("example.net"),
        path=Some("/"),
        expires =Some(new DateTime(2010, 12, 31, 23, 59, 59, DateTimeZone.UTC)),
        version=1,
        httpOnly = true)
    )
  }

  it should "ignore case in known attributes" in {
    val cookies = CookieParser.parseSetCookieHeader("foo=bar; EXPIRES=Fri, 31-Dec-2010 23:59:59 GMT; PATH=/; DOMAIN=.example.net; httponly; SECURE")
    assertSetCookies(cookies,
      CS("foo","bar",
        domain=Some("example.net"),
        path=Some("/"),
        expires =Some(new DateTime(2010, 12, 31, 23, 59, 59, DateTimeZone.UTC)),
        version=1,
        httpOnly = true,
        secure = true)
    )
  }

  it should "ignore path attribute if it doesn't start with a '/'" in {
    val cookies = CookieParser.parseSetCookieHeader("foo=bar; path=foo/bar; domain=.example.net;")
    assertSetCookies(cookies,
      CS("foo","bar",
        domain=Some("example.net"),
        path=None,
        version=1)
    )
  }

  "parseCookieDateTimeString" should "parse dates in pattern " in {
    val expected = Some(new DateTime(2010, 12, 31, 23, 59, 59, DateTimeZone.UTC))
    val actual = CookieParser.Internals.parseCookieDateTimeString("Fri, 31-Dec-2010 23:59:59 GMT")
    actual should equal(expected)
  }

  case class C(name: String, value: String)

  case class CS(name: String, value: String,
                domain: Option[String] = None,
                path: Option[String] = None,
                expires: Option[DateTime] = None,
                version: Int = 0,
                comment: Option[String] = None,
                commentUri: Option[String] = None,
                hostOnly: Boolean = false,
                secure: Boolean = false,
                httpOnly: Boolean = false
               )

  def assertName(cookie: Cookie, name: String) = withClue("Bad Cookie Name:") {
    cookie.name should equal(name)
  }

  def assertValue(cookie: Cookie, value: String) = withClue("Bad Cookie Value:") {
    cookie.value should equal(value)
  }

  def assertCookie(cookie: Cookie, test: C) = {
    assertName(cookie, test.name)
    assertValue(cookie, test.value)
  }

  def assertSetCookie(cookie: Cookie, test: CS) = {
    assertName(cookie, test.name)
    assertValue(cookie, test.value)
    withClue("Bad Domain:") {
      cookie.domain.asOptionalString should equal(test.domain)
    }
    withClue("Bad Path:") {
      cookie.path.asOptionalString should equal(test.path)
    }
    withClue("Bad expires:") {
      cookie.expires should equal(test.expires)
    }
    withClue("Bad hostOnly:") {
      cookie.hostOnly should equal(test.hostOnly)
    }
    withClue("Bad secure:") {
      cookie.secure should equal(test.secure)
    }
    withClue("Bad httpOnly:") {
      cookie.httpOnly should equal(test.httpOnly)
    }
  }

  def assertCookies(cookies: List[Cookie], testCookies: C*) = {
    if (cookies.length != testCookies.length) {
      val failure = cookies.map(x => x.name)
        .zipAll(testCookies.map(x => x.name), "MISSING", "MISSING")
        .zipWithIndex
        .foldLeft(s"Expected ${testCookies.length} cookies, got ${cookies.length}\n") {
          case (acc, ((a, b), i)) => acc + f"[$i%2d] $a%30s | $b%30s%n"
        }
      fail(failure)
    } else cookies.zip(testCookies).zipWithIndex.foreach {
      case ((cookie, test), idx) => withClue(s"Cookie '${cookie.name}' [$idx]:") {
        assertCookie(cookie, test)
      }
    }
  }

  def assertSetCookies(cookies: List[Cookie], testCookies: CS*) = {
    if (cookies.length != testCookies.length) {
      val failure = cookies.map(x => x.name)
        .zipAll(testCookies.map(x => x.name), "MISSING", "MISSING")
        .zipWithIndex
        .foldLeft(s"Expected ${testCookies.length} cookies, got ${cookies.length}\n") {
          case (acc, ((a, b), i)) => acc + f"[$i%2d] $a%30s | $b%30s%n"
        }
      fail(failure)
    } else cookies.zip(testCookies).zipWithIndex.foreach {
      case ((cookie, test), idx) => withClue(s"Cookie '${cookie.name}' [$idx]:") {
        assertSetCookie(cookie, test)
      }
    }
  }

  def assertSingleCookie(cookies: List[Cookie], test: C) = {
    withClue(s"Wrong number of cookies parsed: $cookies:") {
      cookies.length should equal(1)
    }
    assertCookie(cookies.head, test)
  }

}
