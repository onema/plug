package plug

import org.joda.time.DateTime
import StringExtensions.StringEscape
object Cookie {

  /**
    * Gets the matching cookie with the longest path from a collection of cookies.
    *
    * @param cookies Collection of cookies.
    * @param name    Cookie name.
    * @return Matching cookie with longest path, or None if no cookies matched.
    */
  def getCookie(cookies: List[Cookie], name: String): Option[Cookie] = ???

  //
  //    // TODO (steveb): consider making this an extension method
  //
  //    // TODO (arnec): Should also match on domain/path as sanity check
  //    DreamCookie result = null;
  //    int maxPathLength = -1;
  //    foreach(DreamCookie cookie in cookies) {
  //      int length = cookie.Path == null ? 0 : cookie.Path.Length;
  //      if((cookie.Name != name) || (length <= maxPathLength)) {
  //        continue;
  //      }
  //      maxPathLength = length;
  //      result = cookie;
  //    }
  //    return result;
  //  }

  /**
    * Render a collection of cookies into a cookie header.
    *
    * @param cookies Collection of cookies.
    * @return Http cookie header.
    */
  def renderCookieHeader(cookies: List[Cookie]): String = ???

  //    if((cookies == null) || (cookies.Count == 0)) {
  //      return string.Empty;
  //    }
  //    StringBuilder result = new StringBuilder();
  //    result.AppendFormat("$Version=\"{0}\"", 1);
  //    bool first = true;
  //    foreach(DreamCookie cookie in cookies) {
  //
  //      // NOTE (steveb): Dream 1.5 and earlier REQUIRES the $Version value to be separated by a semi-colon (;) instead of a comma (,)
  //
  //      if(first) {
  //        first = false;
  //        result.Append("; ");
  //      } else {
  //        result.Append(", ");
  //      }
  //      result.Append(cookie.ToCookieHeader());
  //    }
  //    return result.ToString();
  //  }

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
    * @param httpOnly   true if cookie is only accessible to the http transport, i.e. not client side scripts.
    * @return New cookie instance.
    */
  def apply(name: String,
            value: String,
            uri: Option[Uri],
            expires: Option[DateTime] = None,
            secure: Boolean = false,
            comment: Option[String] = None,
            commentUri: Option[Uri] = None,
            httpOnly: Boolean = false,
            setCookie: Boolean = true): Cookie = {
    new Cookie(name,
      value,
      uri.map(_.withoutCredentials().withoutQuery().withoutFragment()),
      expires,
      version = if (setCookie) 1 else 0,
      secure = secure,
      comment = comment,
      commentUri = commentUri,
      httpOnly = httpOnly
    )
  }

  //, bool secure, string comment, XUri commentUri, bool httpOnly) {
  //    return new DreamCookie(name, value, uri, expires, 1, secure, false, comment, commentUri, httpOnly, false);
  //  }
  //    _name = name;
  //    _value = value;
  //    if(uri != null) {
  //      _uri = uri.WithoutQuery().WithoutCredentials().WithoutFragment().AsLocalUri();
  //      if(!skipContextDiscovery) {
  //        DreamContext dc = DreamContext.CurrentOrNull;
  //        if(dc != null) {
  //          _publicUri = dc.PublicUri;
  //          _localMachineUri = dc.Env.LocalMachineUri;
  //        }
  //      }
  //    }
  //
  //    // auto-convert very old expiration dates to max since they are most likely bogus
  //    if(expires.Year < 2000) {
  //      expires = DateTime.MaxValue;
  //    }
  //    if(expires != DateTime.MaxValue) {
  //      expires = expires.ToUniversalTime();
  //
  //      // need to trim milliseconds of the passed in date
  //      expires = new DateTime(expires.Year, expires.Month, expires.Day, expires.Hour, expires.Minute, expires.Second, 0, DateTimeKind.Utc).ToUniversalTime();


  /// <returns></returns>
  /**
    * Format [[DateTime]] in standard cookie format.
    *
    * @param date Date.
    * @return Cookie datetime string.
    */
  def formatCookieDateTimeString(date: DateTime): String = ???

  //    return date.ToSafeUniversalTime().ToString("ddd, dd-MMM-yyyy HH:mm:ss 'GMT'", CultureInfo.InvariantCulture.DateTimeFormat);
  //  }

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

  override def toString(): String = {
    val location = uri.map(x => s"@${x.hostPort}${x.path}").getOrElse("")
    s"Cookie($name$location=$value)"
  }

  /**
    * Create new cookie based on current instance with host/port.
    *
    * @param hostPort Host/port to use for new instance.
    * @return New cookie.
    */
  def withHostPort(hostPort: String): Cookie = ??? // Should be with Domain?
  //    string scheme = _uri == null ? "local" : _uri.Scheme;
  //    string path = _uri == null ? string.Empty : _uri.Path;
  //    return new DreamCookie(Name, Value, new XUri(string.Format("{0}://{1}{2}", scheme, hostPort, path)), Expires, Version, Secure, Discard, Comment, CommentUri, HttpOnly, false);
  //  }

  /**
    * Create new cookie based on current instance with path.
    *
    * @param path Path to use for new instance.
    * @return New Cookie.
    */
  def withPath(path: String): Cookie = ???

  //    string scheme = _uri == null ? "local" : _uri.Scheme;
  //    string hostPort = _uri == null ? string.Empty : _uri.Path;
  //    return new DreamCookie(Name, Value, new XUri(string.Format("{0}://{1}{2}", scheme, hostPort, path)), Expires, Version, Secure, Discard, Comment, CommentUri, HttpOnly, false);
  //  }

  /**
    * Create new cookie based on current instance with expiration.
    *
    * @param expires Cookie expiration.
    * @return New Cookie.
    */
  def withExpiration(expires: DateTime): Cookie = ???

  //    return new DreamCookie(Name, Value, Uri, expires, Version, Secure, Discard, Comment, CommentUri, HttpOnly, false);
  //  }

  /**
    * Create new cookie based on current instance with discard flag.
    *
    * @param discard true if the cookie should be discarded.
    * @return New Cookie.
    */
  def withDiscard(discard: Boolean): Cookie = ???

  //    return new DreamCookie(Name, Value, Uri, Expires, Version, Secure, discard, Comment, CommentUri, HttpOnly, false);
  //  }

  /**
    * Create an Http cookie header from the current instance.
    *
    * @return Http cookie header string.
    */
  def toCookieHeader: String = {
    val result = new StringBuilder()
    result.append(s"""$name="${value.escapeString}"""")
    path match {
      case None =>
      case Some(p) => result.append(s"""; $$Path="$p"""")
    }
    domain match {
      case None =>
      case Some("") =>
      case Some(d) => result.append(s"""; $$Domain="$d"""")
    }
    result.toString()
  }

  //
  //    // Note (arnec): We always translate cookies to the public form before serializing
  //    XUri uri = GetPublicUri();
  //    StringBuilder result = new StringBuilder();
  //    result.AppendFormat("{0}=\"{1}\"", Name, Value.EscapeString());
  //    if(!string.IsNullOrEmpty(Path)) {
  //      result.AppendFormat("; $Path=\"{0}\"", uri.Path);
  //    }
  //    if(!string.IsNullOrEmpty(Domain)) {
  //      result.AppendFormat("; $Domain=\"{0}\"", uri.HostPort);
  //    }
  //    return result.ToString();
  //  }

  /**
    * Create an Http set-cookie header from the current instance.
    *
    * @return Http set-cookie header string.
    */
  def toSetCookieHeader: String = ???

  //
  //    // Note (arnec): We always translate cookies to the public form before serializing
  //    XUri uri = GetPublicUri();
  //    StringBuilder result = new StringBuilder(1024);
  //    result.AppendFormat("{0}=\"{1}\"", Name, Value.EscapeString());
  //    if(!string.IsNullOrEmpty(Comment)) {
  //      result.Append("; Comment=\"" + Comment.EscapeString() + "\"");
  //    }
  //    if(CommentUri != null) {
  //      result.Append("; CommentURL=\"" + CommentUri.ToString().EscapeString() + "\"");
  //    }
  //    if(Discard) {
  //      result.Append(result + "; Discard");
  //    }
  //
  //    // Note (arnec): domain names require a dot prefix (so that a cookie can't be set to .com)
  //    //               while hostnames (including localhost) should force Domain to be omitted
  //    //               so that the receiver can appropriately set it
  //    if(!string.IsNullOrEmpty(Domain) && Domain.Contains(".") && !Domain.StartsWith(".")) {
  //      string domain = uri.HostIsIp ? uri.Host : "." + uri.Host;
  //      result.AppendFormat("; Domain={0}", domain);
  //    }
  //    if(Expires < DateTime.MaxValue) {
  //      result.Append("; Expires=" + FormatCookieDateTimeString(Expires));
  //    }
  //
  //    // NOTE (arnec): Do not remove, but re-evaluate, since port is a problem with the standard.
  //    //if(!string.IsNullOrEmpty(Port)) {
  //    //    result.Append("; Port=\"" + Port + "\"");
  //    //}
  //    if(Version > 1) {
  //      result.Append("; Version=\"" + Version.ToString(NumberFormatInfo.InvariantInfo) + "\"");
  //    } else if(Version > 0) {
  //      result.Append("; Version=" + Version.ToString(NumberFormatInfo.InvariantInfo));
  //    }
  //    if(!string.IsNullOrEmpty(Path)) {
  //      result.Append("; Path=" + uri.Path);
  //    }
  //    if(Secure) {
  //      result.Append("; Secure");
  //    }
  //    if(HttpOnly) {
  //      result.Append("; HttpOnly");
  //    }
  //    return result.ToString();
  //  }
}
