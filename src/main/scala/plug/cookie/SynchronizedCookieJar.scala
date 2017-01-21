package plug.cookie

import plug.Uri

class SynchronizedCookieJar(private var domainTree: DomainTree = DomainTree.emptySuffixTree)
                           (implicit publicSuffixes: PublicSuffix) extends CookieJar {

  def count: Int = synchronized {
    domainTree.count
  }

  def empty: Boolean = count == 0

  def clear() = synchronized {
    domainTree = DomainTree.emptySuffixTree
  }

  def toDomainTree = synchronized(this.domainTree)

  def get(uri: Uri): List[Cookie] = synchronized {
    val domain = Domain(uri.host)
    val secureFilter: Cookie => Boolean =
      if (uri.scheme.equalsIgnoreCase("https")) cookie => true
      else cookie => !cookie.secure
    domainTree.get(domain, uri.segments)
      .filter { cookie => !cookie.expired && secureFilter(cookie) && (!cookie.hostOnly ||cookie.domain == domain) }
  }

  def update(cookie: Cookie, uri: Uri): Unit = cookie.validateCookieForUri(uri) map {
    cookie => synchronized {
      domainTree.update(cookie, cookie.domain, cookie.path.segments)
    }
  }
}
