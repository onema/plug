package plug.cookie

import plug.Uri

object CookieJar {

  object Global {

    // TODO: make this configurable to use the resource or network verion of PublicSuffix
    private lazy val _cookieJar = new SynchronizedCookieJar(DomainTree.emptySuffixTree)(PublicSuffix.Implicits.FromResource.publicSuffix)

    implicit def current: CookieJar = synchronized {
      _cookieJar
    }

    def update(cookie: Cookie, uri: Uri): Unit = synchronized {
      _cookieJar.update(cookie, uri)
    }
  }

  def requestHostIsSubDomainOfCookieDomain(host: String, cookieDomain: Domain): Boolean = ???

  def requestPathIsSubDirectoryOfCookiePath(segments: List[String], segments1: List[String]) = ???

  def checkCookieForUpdate(cookie: Cookie, uri: Uri): Option[Cookie] = (cookie.domain match {
    case Domain.empty =>
      // set host labels in cookie
      Some(cookie.withDomain(Some(uri.host)).withHostOnly)

    case domain if requestHostIsSubDomainOfCookieDomain(uri.host, domain) =>
      // check that the cookie domain is a subdomain of
      Some(cookie)
    case _ => None

  }).flatMap {
    cookie => cookie.segments match {
      case None =>

        Some(cookie.withSegments(Some(uri.segments)))
      case Some(segments) if requestPathIsSubDirectoryOfCookiePath(uri.segments, segments) =>

        Some(cookie)

      case _ => None
    }
  }

}

trait CookieJar {
  def count: Int
  def empty: Boolean
  def clear(): Unit
  def get(uri: Uri): List[Cookie]
  def update(cookie: Cookie, uri: Uri): Unit


}

