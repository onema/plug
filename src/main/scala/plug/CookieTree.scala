package plug

object CookieTree {
  val empty = CookieTree(Nil, Map.empty)
}

case class CookieTree(cookies: List[Cookie], subTrees: Map[String, CookieTree]) {
  lazy val count: Int = cookies.length + subTrees.map(_._2.count).sum

  lazy val empty: Boolean = count == 0

  def update(cookie: Cookie, segments: List[String]): CookieTree = {

    def delete(tree: CookieTree, query: List[String]): CookieTree = query match {
      case Nil =>
        // this is the tree for the cookie's path
        tree.copy(cookies = tree.cookies.filterNot(_.name == cookie.name))
      case head :: tail => tree.subTrees.get(head) match {
        case None =>
          // no cookie tree for path, so no cookie to delete
          tree
        case Some(subtree) =>
          tree.copy(subTrees = tree.subTrees + (head -> delete(subtree, tail)))
      }
    }

    def insert(tree: CookieTree, query: List[String]): CookieTree = query match {
      case Nil =>
        // this is the tree for the cookie's path
        tree.copy(cookies = cookie :: tree.cookies.filterNot(_.name == cookie.name))
      case head :: tail =>
        val subtree = insert(tree.subTrees.getOrElse(head, CookieTree(Nil, Map.empty)), tail)
        tree.copy(subTrees = tree.subTrees + (head -> subtree))
    }

    if (cookie.expired) delete(this, segments) else insert(this, segments)
  }

  def get(segments: List[String]): List[Cookie] = {
    def get(tree: CookieTree, query: List[String], acc: List[Cookie]): List[Cookie] = query match {
      case Nil =>
        // this is the most precise match we could get
        tree.cookies ::: acc
      case head :: tail => tree.subTrees.get(head) match {
        case None =>
          // there are no cookies for a more precise match
          tree.cookies ::: acc
        case Some(subtree) =>
          // we can go deeper
          get(subtree, tail, tree.cookies ::: acc)
      }
    }
    get(this, segments, Nil)
  }
}
