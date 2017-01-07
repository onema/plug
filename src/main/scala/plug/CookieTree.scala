package plug

/**
  * Created by arne on 1/7/17.
  */
case class CookieTree(cookies: List[Cookie], subTrees: Map[String, CookieTree]) {
  lazy val count: Int = cookies.length + subTrees.map(_._2.count).sum

  lazy val empty: Boolean = count == 0

  def add(cookie: Cookie, uri: Uri): CookieTree = ???

  def get(query: List[String]): List[Cookie] = {
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
    get(this, query, Nil)
  }
}
