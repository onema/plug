package plug

object CookieJar {

  object Global {
    private val _cookieJar = new CookieJar(Map.empty)

    implicit def current: CookieJar = synchronized {
      _cookieJar
    }

    def update(cookie: Cookie, uri: Uri): Unit = synchronized {
      _cookieJar.update(cookie, uri)
    }
  }

}

class CookieJar(var domainTrees: Map[String, CookieTree]) {

  def count: Int = synchronized {
    domainTrees.map(_._2.count).sum
  }

  def empty: Boolean = count == 0

  def clear() = synchronized {
    domainTrees = Map.empty
  }

  def get(uri: Uri): List[Cookie] = synchronized {
    // Note: this could be made more efficient by only wrapping the retrieval of the domainTree in synchronized
    domainTrees.get(uri.host) match {
      case None => Nil
      case Some(tree) =>
        val matches = tree.get(uri.segments)
        uri.scheme.equalsIgnoreCase("https") match {
          case true => matches // https gets all cookies
          case false => matches.filterNot(_.secure) // http only gets cookies not marked secure
        }
    }
  }

  def update(cookie: Cookie, uri: Uri): Unit = (cookie.uri match {
    case None => true
    // Need to check that the path is legal for the cookie origin
    case Some(cookieUri) => cookieUri.host.equalsIgnoreCase(uri.host)
  }) match {
    case false =>
    case true => synchronized {
      val tree = domainTrees.getOrElse(uri.host, CookieTree.empty).add(cookie, uri.segments)
      domainTrees = domainTrees + (uri.host -> tree)
    }
  }

}
