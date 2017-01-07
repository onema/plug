package plug

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import plug.StringExtensions.StringEscape

object Cookie {

  // TODO: Really should break this into Cookie and SetCookie, at the very least at the object level
  // TODO: Reconsider using Uri to represent Domain and Path internally

  /**
    * Gets the matching cookie with the longest path from a collection of cookies.
    *
    * @param cookies Collection of cookies.
    * @param name    Cookie name.
    * @return Matching cookie with longest path, or None if no cookies matched.
    */
  def getCookie(cookies: List[Cookie], name: String): Option[Cookie] = cookies.foldLeft[(Option[Cookie], Int)]((None, -1)) {
    case ((result, maxPathLength), cookie) =>

      // TODO (arnec): Should also match on domain/path as sanity check
      val length = cookie.path.map(_.length).getOrElse(0)
      if (cookie.name != name || length <= maxPathLength) {
        (result, maxPathLength)
      } else {
        (Some(cookie), length)
      }
  }._1

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
    * @param name       Cookie name.
    * @param value      Cookie value.
    * @param uri        Cookie Uri.
    * @param expires    Cookie expiration.
    * @param secure     true if the cookie should only be used on https requests.
    * @param comment    Comment.
    * @param commentUri Uri for comment.
    * @param version    Cookie version (if omitted set to 0 for regular and 1 for set cookies).
    * @param discard    true if the cookie is to be discarded.
    * @param httpOnly   true if cookie is only accessible to the http transport, i.e. not client side scripts.
    * @param setCookie  true is this is a set cookie (sets version to 1).
    * @return New cookie instance.
    */
  def apply(name: String,
            value: String,
            uri: Option[Uri] = None,
            expires: Option[DateTime] = None,
            secure: Boolean = false,
            comment: Option[String] = None,
            commentUri: Option[Uri] = None,
            version: Option[Int] = None,
            discard: Boolean = false,
            httpOnly: Boolean = false,
            setCookie: Boolean = true): Cookie = {
    new Cookie(name,
      value,
      uri.map(_.withoutCredentials().withoutQuery().withoutFragment().withPort(80).withScheme("http")),
      validateExpire(expires),
      version = version.getOrElse(if (setCookie) 1 else 0),
      secure = secure,
      comment = comment,
      commentUri = commentUri,
      httpOnly = httpOnly
    )
  }

  private def validateExpire(expires: Option[DateTime]) = expires.flatMap { e =>
    // very old dates are treated as bogus and converted to None
    if (e.year().get < 2000) None
    // while every other date needs to be set to UTC and milliseconds trimmed off
    else Some(e.toDateTime(DateTimeZone.UTC).withMillisOfSecond(0))
  }

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
  *
  * @param name       Cookie name.
  * @param value      Cookie value.
  * @param uri        Cookie Uri.
  * @param expires    Cookie expiration.
  * @param version    Cookie version.
  * @param secure     true if the cookie is used on Https only.
  * @param discard    true if the cookie is to be discarded.
  * @param comment    Cookie comment.
  * @param commentUri Cookie comment uri.
  * @param httpOnly   true if cookie is only accessible to the http transport, i.e. not client side scripts.
  */
class Cookie private(val name: String,
                     val value: String,
                     val uri: Option[Uri],
                     val expires: Option[DateTime] = None,
                     val version: Int = 0,
                     val secure: Boolean = false,
                     val discard: Boolean = false,
                     val comment: Option[String] = None,
                     val commentUri: Option[Uri] = None,
                     val httpOnly: Boolean = false) {

  /** Domain of the cookie. */
  def domain = uri.map(_.hostPort)

  /** Cookie Path. */
  def path = uri.map(_.path)

  /** true if the cookie is already expired. */
  def expired = expires.exists(_.isBeforeNow)

  /** hashCode based soley on [[name]], [[value]] & [[uri]]. */
  override def hashCode(): Int = List(name, value, uri)
    .foldLeft(0) { case (acc, x) => acc * 41 + x.hashCode() }

  /**
    * Compare instances for identical contents.
    *
    * <p>[[Cookie]] instances are considered identical based solely on [[name]], [[value]] & [[uri]].
    *
    * @param obj Other instance.
    * @return
    */
  override def equals(obj: Any) = obj match {
    case c: Cookie => name == c.name && value == c.value && uri == c.uri
    case _ => false
  }

  override def toString: String = {
    val location = uri.map(x => s"@${x.hostPort}${x.path}").getOrElse("")
    s"Cookie($name$location=$value)"
  }

  private def copy(name: String = name,
                   value: String = value,
                   uri: Option[Uri] = uri,
                   expires: Option[DateTime] = expires,
                   version: Int = version,
                   secure: Boolean = secure,
                   discard: Boolean = discard,
                   comment: Option[String] = comment,
                   commentUri: Option[Uri] = commentUri,
                   httpOnly: Boolean = httpOnly) =
    new Cookie(name, value, uri, expires, version, secure, discard, comment, commentUri, httpOnly)

  /**
    * Create new cookie based on current instance with domain.
    *
    * @param domain Domain to use for new instance.
    * @return New cookie.
    */
  def withDomain(domain: String): Cookie = copy(uri = uri match {
    case None => Uri.fromString(s"http://$domain") // Note: this silently fails to None if the domain is not legal
    case Some(u) => Some(u.withHost(domain))
  })

  /**
    * Create new cookie based on current instance with path.
    *
    * @param path Path to use for new instance. The path must start with '/'
    * @return New Cookie.
    */
  def withPath(path: String): Cookie = copy(uri = uri match {
    case None => Uri.fromString(s"http://$path")
    case Some(u) => Some(u.atAbsolutePath(path))
  })

  /**
    * Create new cookie based on current instance with expiration.
    *
    * @param expires Cookie expiration.
    * @return New Cookie.
    */
  def withExpiration(expires: DateTime): Cookie = copy(expires = Cookie.validateExpire(Some(expires)))

  /**
    * Create new cookie based on current instance with discard flag.
    *
    * @param discard true if the cookie should be discarded.
    * @return New Cookie.
    */
  def withDiscard(discard: Boolean): Cookie = copy(discard = discard)

  /**
    * Create an Http cookie header from the current instance.
    *
    * @return Http cookie header string.
    */
  def toCookieHeader: String = {
    val result = new StringBuilder()
    result.append(s"""$name="${value.escapeString}"""")
    path.foreach(x => result.append(s"""; $$Path="$x""""))
    domain.filter(!_.isEmpty).foreach(x => result.append(s"""; $$Domain="$x""""))
    result.toString()
  }

  /**
    * Create an Http set-cookie header from the current instance.
    *
    * @return Http set-cookie header string.
    */
  def toSetCookieHeader: String = {
    val result = new StringBuilder()
    result.append(s"""$name="${value.escapeString}"""")
    comment.foreach(x => result.append(s"""; Comment=${x.escapeString}""""))
    commentUri.foreach(x => result.append(s"""; Comment=${x.toUriString.escapeString}""""))
    if(discard) result.append("; Discard")
    path.foreach(x => result.append(s"""; $$Path="$x""""))
    uri.filter(x => ! x.host.isEmpty && x.host.contains('.') && !x.host.startsWith(".")).foreach { u =>
      val d = if(u.hostIsIp) u.host else s".${u.host}"
      result.append(s"""; $$Domain="$d"""")
    }
    expires.map(Cookie.formatCookieDateTimeString).foreach(x => result.append(s"""; Expires=$x"""))
    if(version > 1 ) { result.append(s"""; Version="$version"""")}
    else if(version > 0){ result.append(s"""; Version=$version""")}
    if(secure){ result.append("; Secure")}
    if(httpOnly){ result.append("; HttpOnly")}
    result.toString()
  }
}
