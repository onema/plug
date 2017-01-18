package plug.cookie

import plug.Uri

object NoCookieJar {
  object Implicits {
    implicit def cookieJar = new NoCookieJar
  }
}

class NoCookieJar extends CookieJar {
  override def count: Int = 0

  override def update(cookie: Cookie, uri: Uri): Unit = {}

  override def get(uri: Uri): List[Cookie] = Nil

  override def clear(): Unit = {}

  override def empty: Boolean = true
}
