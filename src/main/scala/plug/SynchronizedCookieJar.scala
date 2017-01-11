package plug


class SynchronizedCookieJar(private var domainTree: DomainTree)(implicit publicSuffixes: PublicSuffix) extends CookieJar {

  def count: Int = synchronized {
    domainTree.count
  }

  def empty: Boolean = count == 0

  def clear() = synchronized {
    domainTree = DomainTree.empty
  }

  def toDomainTree = synchronized(this.domainTree)

  def get(uri: Uri): List[Cookie] = ??? //synchronized {
//    domainTrees.filter(_.)
//    // Note: this could be made more efficient by only wrapping the retrieval of the domainTree in synchronized
//    domainTrees.get(uri.host) match {
//      case None => Nil
//      case Some(tree) =>
//        val matches = tree.get(uri.segments)
//        uri.scheme.equalsIgnoreCase("https") match {
//          case true => matches // https gets all cookies
//          case false => matches.filterNot(_.secure) // http only gets cookies not marked secure
//        }
//    }
//  }

  def update(cookie: Cookie, uri: Uri): Unit = ??? //(cookie.hostLabels match {
//    case Nil =>
//      // set uri in cookie
//      Some(cookie.withDomain(Some(uri.host)))
//
//    case Some(hostLabels) if(isDomainMatch(hostLabels,Cookie.Internals.toDomainLabels(Some(uri.host))) cookie else
//        case None =>
//          // set domain in cookie
//          Some(cookie.withDomain(uri.host))
//        case Some(domain) if domain.equalsIgnoreCase(uri.host) => // TODO: Domain matching seems wrong.. what about foo.example.com & example.com?
//          // cookie has proper host
//          Some(cookie)
//        case _ =>
//          // ignore cookie
//          None
//
//      }).flatMap { cookie => cookie.segments match {
//      case None =>
//        // set path in cookie
//        Some(cookie.withSegments(uri.segments))
//      case Some(segments) if uri.pathStartsWith(segments) =>
//        // cookie has proper path
//        Some(cookie)
//      case _ =>
//        // ignore cookie
//        None
//    }}
//  }) match {
//    case None => // ignoring cookie
//    case Some(cookie1) => synchronized {
//      val tree = domainTrees.getOrElse(cookie1.domain.get, CookieTree.empty).update(cookie1, cookie1.segments.get)
//      domainTrees = domainTrees + (uri.host -> tree)
//    }
//  }

}
