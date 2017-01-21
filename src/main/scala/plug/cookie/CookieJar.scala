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
}

trait CookieJar {
  def count: Int
  def empty: Boolean
  def clear(): Unit
  def get(uri: Uri): List[Cookie]
  def update(cookie: Cookie, uri: Uri): Unit


}

