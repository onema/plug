package plug


class SynchronizedCookieJar(private var domainTree: DomainTree)(implicit publicSuffixes: PublicSuffix) extends CookieJar {

  def count: Int = synchronized {
    domainTree.count
  }

  def empty: Boolean = count == 0

  def clear() = synchronized {
    domainTree = DomainTree.emptySuffixTree
  }

  def toDomainTree = synchronized(this.domainTree)

  def getHostnameLabels(uri: Uri): List[String] =
    if(uri.hostIsIp) List(uri.host)
    else uri.host.split('.').reverse.toList



  def get(uri: Uri): List[Cookie] = synchronized {
    domainTree.get(getHostnameLabels(uri),uri.segments)
  }

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
