package plug.cookie

/**
  * Created by arne on 1/11/17.
  */
object SuffixNode {
  val empty = SuffixNode(Map.empty)
}

// TODO need to handle the '*' case
case class SuffixNode(children: Map[String, SuffixNode]) {
  def addSuffix(suffix: List[String]): SuffixNode = {
    def add(node: SuffixNode, parts: List[String]): SuffixNode = parts match {
      case Nil => node
      case head :: Nil =>
        // we've exhausted the suffix we're adding
        node.children.get(head) match {
          // the part does not exist in tree yet, add and return
          case None => SuffixNode(node.children + (head -> SuffixNode.empty))
          // the part does exist in tree, just return the node
          case Some(child) => node
        }
      case head :: tail =>
        // we've not exhausted the suffix yet
        node.children.get(head) match {
          // the part does not exist in the tree yet, add and recurse on tail
          case None => SuffixNode(node.children + (head -> add(SuffixNode.empty, tail)))
          // the part already exists, recurse on tail
          case Some(child) =>
            add(child, tail) match {
              // child was unchanged, tree is unchanged
              case `child` => node
              case newChild => SuffixNode(node.children + (head -> newChild))
            }
        }
    }
    add(this, suffix)
  }

  def contains(suffix: List[String]): Boolean = {
    def contains(node: SuffixNode, parts: List[String]): Boolean = parts match {
      case Nil => true
      case head:: tail => node.children.get(head) match {
        case None => false
        case Some(child) => contains(child, tail)
      }
    }
    contains(this, suffix)
  }

  def splitOnSuffix(labels: List[String]): (List[String],List[String]) = {
    def splitOnSuffix(node: SuffixNode, prefix: List[String], suffix:List[String]): (List[String],List[String]) =
      prefix match {
        case Nil => (Nil,suffix)
        case head::tail => node.children.get(head) match {
          case None => (prefix,suffix)
          case Some(child) => splitOnSuffix(child,tail,head::suffix)
        }
      }
    val (prefix, suffix)= splitOnSuffix(this,labels,Nil)
    (prefix, suffix.reverse)
  }
}
