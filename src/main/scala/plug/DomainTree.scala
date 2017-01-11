package plug

object DomainTree {
  val empty = DomainTree(None,Map.empty)
}

case class DomainTree(cookies: Option[CookieTree], subTrees: Map[String, DomainTree]) {

  lazy val count: Int = cookies.map(_.count).sum + subTrees.map(_._2.count).sum

  lazy val empty: Boolean = count == 0

  def get(labels: List[String], segments: List[String]): List[Cookie] = {
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
    get(this, labels, Nil)
  }
}