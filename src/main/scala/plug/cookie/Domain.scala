package plug.cookie

import plug.Uri

object Domain {
  val empty = Domain(Nil)

  def apply(host: Option[String]): Domain = host.map(Domain(_)).getOrElse(empty)

  def apply(host: String): Domain = host match {
    case d if d.isEmpty => empty
    // TODO: what about IPv6?
    case d if Uri.Internals.isIp(d) => Domain(List(d))
    case d => Domain(d.split('.').toList.filter(!_.isEmpty).reverse)
  }
}
case class Domain(parts: List[String]) {
  def isEmpty = parts.isEmpty
  lazy val optionalDomainString = parts match {
    case Nil => None
    case _ => Some(String.join(".", parts.reverse: _*))
  }
}
