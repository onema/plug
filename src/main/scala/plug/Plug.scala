package plug

trait RequestBody

object EmptyRequestBody extends RequestBody

object Plug {
  def apply(uri: Uri)(implicit cookieJar: CookieJar): Plug = new Plug(uri, Nil, cookieJar)

  def apply(uri: String)(implicit cookieJar: CookieJar): Option[Plug] = Uri.fromString(uri).map(new Plug(_, Nil, cookieJar))
}

class Plug(val uri: Uri, val headers: List[(String, String)], val cookieJar: CookieJar, requestBody: RequestBody = EmptyRequestBody) {

}
