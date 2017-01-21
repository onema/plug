package plug.cookie

import plug.Uri

trait Domain {
  def isEmpty: Boolean

  def isSubDomainOf(domain: Domain): Boolean

  def asOptionalString: Option[String]

  def parts: List[String]
}

object Domain {
  val empty: Domain = DomainImpl(Nil)

  def apply(host: Option[String]): Domain = host.map(Domain(_)).getOrElse(empty)

  def apply(host: String): Domain = host match {
    case d if d.isEmpty => empty
    // TODO: what about IPv6?
    case d if Uri.Internals.isIp(d) => Domain(List(d))
    case d => Domain(d.split('.').toList.filter(!_.isEmpty).map(_.toLowerCase).reverse)
  }

  def apply(parts: List[String]): Domain = DomainImpl(parts.map(_.toLowerCase))

  private case class DomainImpl(parts: List[String]) extends Domain {
    def isSubDomainOf(domain: Domain) = if (domain.isEmpty) false else parts.startsWith(domain.parts)

    def isEmpty = parts.isEmpty

    lazy val asOptionalString = parts match {
      case Nil => None
      case _ => Some(String.join(".", parts.reverse: _*))
    }
  }

}

