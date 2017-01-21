package plug.cookie

import plug.UriParser

trait Path {
  def isEmpty: Boolean

  def isSubPathOf(path: Path): Boolean

  def asOptionalString: Option[String]

  def segments: List[String]
}

object Path {

  val empty = new Path {
    val isEmpty = true

    def isSubPathOf(path: Path) = false

    val asOptionalString = None
    val segments = Nil
  }

  def apply(path: Option[String]): Path = path.map(Path(_)).getOrElse(empty)

  def apply(path: String): Path =
    if (path == null || path.isEmpty) empty
    else {
      // TODO: run only the parts of UriParser needed for path
      UriParser.tryParse(s"http://$path").map(x => NonEmptyPath(x.segments.map(_.toLowerCase))).getOrElse(empty)
    }

  def apply(segments: List[String]): Path = NonEmptyPath(segments.map(_.toLowerCase))


  private case class NonEmptyPath(segments: List[String]) extends Path {

    def isEmpty = false

    def isSubPathOf(path: Path) = if (path.isEmpty) false else segments.startsWith(path.segments)

    lazy val asOptionalString: Option[String] = Some("/" + String.join("/", segments: _*))
  }

}
