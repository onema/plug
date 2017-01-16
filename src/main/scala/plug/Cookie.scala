package plug

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import plug.StringExtensions.StringEscape

object Cookie {

  // TODO: Really should break this into Cookie and SetCookie, at the very least at the object level

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
    * @param domain     Cookie domain.
    * @param path       Cookie path.
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
            domain: Option[String] = None,
            path: Option[String] = None,
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
      Internals.toDomainLabels(domain),
      Internals.toSegments(path),
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

  object Internals {
    def toSegments(path: Option[String]): Option[List[String]] =
    // TODO: run only the parts of UriParser needed for path
      path.flatMap(x => UriParser.tryParse(s"http://$x")).map(_.segments)


    def toDomainLabels(domain: Option[String]): List[String] = domain match {
      case None => Nil
      case Some(d) if d.isEmpty => Nil
      // TODO: should validate domain as hostname and strip leading '.'
      case Some(d) => if(Uri.Internals.isIp(d)) List(d)
      else d.split('.').toList.filter(!_.isEmpty)
    }
  }

}

/**
  * A case class-like representation of an Http Cookie
  *
  * @param name         Cookie name.
  * @param value        Cookie value.
  * @param hostLabels Cookie Domain.
  * @param segments     Cookie path segments. Some(Nil) denotes root path '/'
  * @param version      Cookie version.
  * @param secure       true if the cookie is used on Https only.
  * @param discard      true if the cookie is to be discarded.
  * @param comment      Cookie comment.
  * @param commentUri   Cookie comment uri.
  * @param httpOnly     true if cookie is only accessible to the http transport, i.e. not client side scripts.
  */
class Cookie private(val name: String,
                     val value: String,
                     val hostLabels: List[String],
                     val segments: Option[List[String]],
                     val expires: Option[DateTime] = None,
                     val version: Int = 0,
                     val secure: Boolean = false,
                     val discard: Boolean = false,
                     val comment: Option[String] = None,
                     val commentUri: Option[Uri] = None,
                     val httpOnly: Boolean = false) {

  /** Cookie Domain. */
  lazy val domain: Option[String] = hostLabels match {
    case Nil => None
    case _ => Some(String.join(".", hostLabels: _*))
  }

  /** Cookie Path. */
  lazy val path: Option[String] = segments.map("/" + String.join("/", _: _*))

  /** true if the cookie is already expired. */
  def expired = expires.exists(_.isBeforeNow)

  /** hashCode based soley on [[name]], [[value]], [[hostLabels]] & [[segments]]. */
  override def hashCode(): Int = List(name, value, hostLabels, segments)
    .foldLeft(0) { case (acc, x) => acc * 41 + x.hashCode() }

  /**
    * Compare instances for identical contents.
    *
    * <p>[[Cookie]] instances are considered identical based solely on [[name]], [[value]], [[hostLabels]] & [[segments]].
    *
    * @param obj Other instance.
    * @return
    */
  override def equals(obj: Any) = obj match {
    case c: Cookie => name == c.name && value == c.value && hostLabels == c.hostLabels && segments == c.segments
    case _ => false
  }

  override lazy val toString: String = {
    val uri = s"${domain.getOrElse("")}${path.getOrElse("")}"
    val location = if (uri.isEmpty) "" else s"@$uri"
    s"Cookie($name$location=$value)"
  }

  private def copy(name: String = name,
                   value: String = value,
                   domainLabels: List[String] = hostLabels,
                   segments: Option[List[String]] = segments,
                   expires: Option[DateTime] = expires,
                   version: Int = version,
                   secure: Boolean = secure,
                   discard: Boolean = discard,
                   comment: Option[String] = comment,
                   commentUri: Option[Uri] = commentUri,
                   httpOnly: Boolean = httpOnly) =
    new Cookie(name, value, domainLabels, segments, expires, version, secure, discard, comment, commentUri, httpOnly)


  /**
    * Create new cookie based on current instance with domain.
    *
    * @param domain Domain to use for new instance.
    * @return New cookie.
    */
  def withDomain(domain: Option[String]): Cookie = copy(domainLabels = Cookie.Internals.toDomainLabels(domain))

  /**
    * Create new cookie based on current instance with path.
    *
    * @param path Path to use for new instance. The path must start with '/'
    * @return New Cookie.
    */
  def withPath(path: Option[String]): Cookie = copy(segments = Cookie.Internals.toSegments(path))


  /**
    * Create new cookie based on current instance path segments.
    *
    * @param segments Path segments to replace current segments (if any) with.
    * @return
    */
  def withSegments(segments: Option[List[String]]): Cookie = copy(segments = segments)

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
    comment.foreach(x => result.append(s"""; Comment=${x.escapeString}""""))
    commentUri.foreach(x => result.append(s"""; Comment=${x.toUriString.escapeString}""""))
    if (discard) result.append("; Discard")
    path.foreach(x => result.append(s"""; Path="$x""""))
    domain.foreach(x => result.append(s"""; Domain="$x""""))
    expires.map(Cookie.formatCookieDateTimeString).foreach(x => result.append(s"""; Expires=$x"""))
    if (version > 1) {
      result.append(s"""; Version="$version"""")
    }
    else if (version > 0) {
      result.append(s"""; Version=$version""")
    }
    if (secure) {
      result.append("; Secure")
    }
    if (httpOnly) {
      result.append("; HttpOnly")
    }
    result.toString()
  }
}
