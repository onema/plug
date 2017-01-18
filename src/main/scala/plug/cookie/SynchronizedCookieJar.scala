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
    val cookies = domainTree.get(Domain(uri.host), uri.segments)
    if(uri.scheme.equalsIgnoreCase("https")) cookies else cookies.filterNot(_.secure)
  }

  def update(cookie: Cookie, uri: Uri): Unit = CookieJar.checkCookieForUpdate(cookie, uri) map {
    cookie => synchronized {
      domainTree.update(cookie, cookie.domain, cookie.segments.get)
    }
  }
}
