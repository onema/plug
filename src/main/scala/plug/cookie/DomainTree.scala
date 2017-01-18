package plug.cookie

object DomainTree {
  val emptyPrefixTree = DomainTree(Some(PathTree.empty), Map.empty)
  val emptySuffixTree = DomainTree(None, Map.empty)
}

case class DomainTree(cookies: Option[PathTree], subTrees: Map[String, DomainTree]) {

  lazy val count: Int = cookies.map(_.count).sum + subTrees.map(_._2.count).sum

  lazy val empty: Boolean = count == 0

  def get(domain: Domain, segments: List[String]): List[Cookie] = {
    def get(tree: DomainTree, query: List[String], acc: List[Cookie]): List[Cookie] = {
      def gatherCookies: List[Cookie] = tree.cookies.map(_.get(segments)).getOrElse(Nil) ::: acc
      query match {
        case Nil =>
          // this is the most precise match we could get
          gatherCookies
        case head :: tail => tree.subTrees.get(head) match {
          case None =>
            // there are no cookies for a more precise match
            gatherCookies
          case Some(subtree) =>
            // we can go deeper
            get(subtree, tail, gatherCookies)
        }
      }
    }
    get(this, domain.parts, Nil)
  }

  def update(cookie: Cookie, domain: Domain, segments: List[String])(implicit suffixTree: PublicSuffix): DomainTree = {
    val (prefix, suffix) = suffixTree.splitOnSuffix(domain.parts)
    if (prefix.isEmpty) {
      // there is no non-public part to the domain, which means the cookie cannot be set
      this
    } else {

      def buildSuffix(tree: DomainTree, query: List[String]): Option[DomainTree] = query match {
        case Nil => Some(tree) // reached the end of the suffix
        case head::tail => tree.subTrees.get(head) match {
          case None if cookie.expired => None // suffix doesn't exit, and the cookie is expired, so we're done
          case None => buildSuffix(DomainTree.emptySuffixTree,tail) match {
            case None => None
            case Some(subtree) => Some(tree.copy(subTrees = tree.subTrees + (head -> subtree)))
          }
          case Some(subtree) => buildSuffix(subtree,tail) match {
            case None => None
            case Some(subtree2) => Some(tree.copy(subTrees = tree.subTrees + (head -> subtree2)))
          }
        }
      }

      def update(tree: DomainTree, query: List[String]): DomainTree = query match {
        case Nil =>
          // this is the tree for the cookie's domain
          tree.cookies match {
            case None => tree // None means we're not allowed to store cookies at this domain part
            case Some(cookieTree) => tree.copy(cookies = Some(cookieTree.update(cookie, segments)))
          }
        case head :: tail => tree.subTrees.get(head) match {
          case None =>
            // no domain tree for domain
            if (cookie.expired) tree
            else tree.copy(subTrees = tree.subTrees + (head -> update(DomainTree.emptyPrefixTree, tail)))
          case Some(subtree) =>
            // TODO: optimize by comparing the updated subtree to the one passed in to see if anything changed
            tree.copy(subTrees = tree.subTrees + (head -> update(subtree, tail)))
        }
      }

      (suffix match {
        case Nil => Some(this) // no public suffix, we can just update the current tree
        case _ => buildSuffix(this, suffix)
      }) match {
        case None => this // determined there was nothing to update
        case Some(updateRoot) => update(updateRoot, domain.parts)
      }
    }
  }
}