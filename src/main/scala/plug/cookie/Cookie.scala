package plug.cookie

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import plug.StringExtensions.StringEscape
import plug.Uri

object Cookie {

  // TODO: Really should break this into Cookie and SetCookie, at the very least at the object level
  // TODO: Remove comment, commentUri, discard and version
  // TODO: Add max-age
  /**
    * Render a collection of cookies into a cookie header.
    *
    * @param cookies Collection of cookies.
    * @return Http cookie header.
    */
  def renderCookieHeader(cookies: List[Cookie]): String = if (cookies.isEmpty) ""
  else {
    val result = new StringBuilder()
    result.append("""$Version="1"""")
    cookies.foreach { cookie =>
      result.append(", ")
      result.append(cookie.toCookieHeader)
    }
    result.toString()
  }

  /**
    * Create a new set cookie.
    *
    * @param name             Cookie name.
    * @param value            Cookie value.
    * @param domain           Cookie domain.
    * @param path             Cookie path.
    * @param expiresOrMaxAge  Cookie expiration.
    * @param secure           true if the cookie should only be used on https requests.
    * @param httpOnly         true if cookie is only accessible to the http transport, i.e. not client side scripts.
    * @param hostOnly         true is cookie is only accessible from the host that set it.
    * @return                 New cookie instance.
    */
  def apply(name: String,
            value: String,
            domain: Option[String] = None,
            path: Option[String] = None,
            expiresOrMaxAge: Option[Either[DateTime,Long]] = None,
            secure: Boolean = false,
            httpOnly: Boolean = false,
            hostOnly: Boolean = false): Cookie = {
    new Cookie(name,
      value,
      Domain(domain),
      Path(path),
      validateExpiration(expiresOrMaxAge),
      secure = secure,
      httpOnly = httpOnly,
      hostOnly = hostOnly
    )
  }

  private def validateExpiration(expires: Option[Either[DateTime,Long]]): Option[DateTime] = ???
//  expires.flatMap { e =>
//    // very old dates are treated as bogus and converted to None
//    if (e.year().get < 2000) None
//    // while every other date needs to be set to UTC and milliseconds trimmed off
//    else Some(e.toDateTime(DateTimeZone.UTC).withMillisOfSecond(0))
//  }

  /**
    * Format [[DateTime]] in standard cookie format.
    *
    * @param date Date.
    * @return Cookie datetime string.
    */
  def formatCookieDateTimeString(date: DateTime): String =
  // TODO: This seems wrong, since GMT and UTC are not identical. Is that an error in the spec or this code?
    date.toDateTime(DateTimeZone.UTC).toString(DateTimeFormat.forPattern("EEE, dd-MMM-yyyy HH:mm:ss 'GMT'"))

}

/**
  * A case class-like representation of an Http Cookie
  *
  * @param name       Cookie name.
  * @param value      Cookie value.
  * @param domain     Cookie Domain.
  * @param path       Cookie path segments. Some(Nil) denotes root path '/'.
  * @param expires    DateTime at which the cookie expires.
  * @param secure     true if the cookie is used on Https only.
  * @param persistent true if the cookie is to be persisted.
  * @param httpOnly   true if cookie is only accessible to the http transport, i.e. not client side scripts.
  * @param hostOnly   true if cookie is only returned to the specific host.
  */
class Cookie private(val name: String,
                     val value: String,
                     val domain: Domain,
                     val path: Path,
                     val expires: Option[DateTime] = None,
                     val secure: Boolean = false,
                     val persistent: Boolean = false,
                     val httpOnly: Boolean = false,
                     val hostOnly: Boolean = false) {

  /** true if the cookie is already expired. */
  def expired = expires.exists(_.isBeforeNow)

  /** hashCode based soley on [[name]], [[value]], [[domain]] & [[path]]. */
  override def hashCode(): Int = List(name, value, domain, path)
    .foldLeft(0) { case (acc, x) => acc * 41 + x.hashCode() }

  /**
    * Compare instances for identical contents.
    *
    * <p>[[Cookie]] instances are considered identical based solely on [[name]], [[value]], [[domain]] & [[path]].
    *
    * @param obj Other instance.
    * @return
    */
  override def equals(obj: Any) = obj match {
    case c: Cookie => name == c.name && value == c.value && domain == c.domain && path == c.path
    case _ => false
  }

  override lazy val toString: String = {
    val uri = s"${domain.asOptionalString.getOrElse("")}${path.asOptionalString.getOrElse("")}"
    val location = if (uri.isEmpty) "" else s"@$uri"
    s"Cookie($name$location=$value)"
  }

  private def copy(name: String = name,
                   value: String = value,
                   domain: Domain = domain,
                   path: Path = path,
                   expires: Option[DateTime] = expires,
                   secure: Boolean = secure,
                   persistent: Boolean = persistent,
                   httpOnly: Boolean = httpOnly,
                   hostOnly: Boolean = hostOnly) =
    new Cookie(name, value, domain, path, expires, secure, persistent, httpOnly, hostOnly)


  /**
    * Create new cookie based on current instance with domain.
    *
    * @param domain Domain to use for new instance.
    * @return New cookie.
    */
  def withDomain(domain: Option[String]): Cookie = copy(domain = Domain(domain))

  def withDomain(domain: Domain): Cookie = copy(domain = domain)

  def withoutDomain: Cookie = copy(domain = Domain.empty)
  /**
    * Create new cookie based on current instance with path.
    *
    * @param path Path to use for new instance. The path must start with '/'
    * @return New Cookie.
    */
  def withPath(path: String): Cookie = copy(path = Path(path))

  def withPath(path: Path): Cookie = copy(path = path)

  def withoutPath: Cookie = copy(path = Path.empty)

  /**
    * Create new cookie based on current instance with expiration.
    *
    * @param expires Cookie expiration.
    * @return New Cookie.
    */
  def withExpiration(expires: DateTime): Cookie = copy(expires = Cookie.validateExpiration(Some(Left(expires))))

  /**
    * Create new cookie based on current instance with persistence flag set
    *
    * @return New Cookie.
    */
  def withPersistence: Cookie = copy(persistent = true)

  def withoutPersistence: Cookie = copy(persistent = false)

  def withHostOnly: Cookie = copy(hostOnly = true)

  def withoutHostOnly: Cookie = copy(hostOnly = false)

  // TODO: should also set expiration, if un-set
  def validateCookieForUri(uri: Uri): Option[Cookie] = (domain match {
    case Domain.empty => Some(withDomain(Some(uri.host)).withHostOnly)
    case _ if Domain(uri.host).isSubDomainOf(domain) => Some(this)
    case _ => None

  }).map {
    cookie => if(cookie.path.isEmpty) cookie.withPath(Path(uri.segments)) else cookie
  }

  /**
    * Create an Http cookie header from the current instance.
    *
    * @return Http cookie header string.
    */
  def toCookieHeader: String =
    s"""$name="${value.escapeString}""""

  /**
    * Create an Http set-cookie header from the current instance.
    *
    * @return Http set-cookie header string.
    */
  def toSetCookieHeader: String = {
    val result = new StringBuilder()
    result.append(s"""$name="${value.escapeString}"""")
    path.asOptionalString.foreach(x => result.append(s"""; Path="$x""""))
    domain.asOptionalString.foreach(x => result.append(s"""; Domain="$x""""))
    expires.map(Cookie.formatCookieDateTimeString).foreach(x => result.append(s"""; Expires=$x"""))
    if (secure) {
      result.append("; Secure")
    }
    if (httpOnly) {
      result.append("; HttpOnly")
    }
    result.toString()
  }
}
