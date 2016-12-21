package plug

/**
  * Created by arne on 12/19/16.
  */
case class Uri(scheme: String,
               user: Option[String] = None,
               password: Option[String] = None,
               hostname: Option[String] = None, // When would this be None and not ""
               port: Option[Int] = None,
               segments: List[String] = Nil,
               query: List[(String, Option[String])] = Nil, // Should it be (String, Option[String])?
               fragment: Option[String] = None,
               usesDefaultPort: Boolean = true,
               trailingSlash: Boolean = false)
