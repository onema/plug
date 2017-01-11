package plug

import java.nio.charset.StandardCharsets

import plug.UriEncoding.UriEncoding

/** Uri Path string encoding format */
object UriPathFormat {

  sealed trait UriPathFormat

  /** Leave path segment as is */
  case object Original extends UriPathFormat

  /** Uri decode path segment  */
  case object Decoded extends UriPathFormat

  /** Normalize encoding */
  case object Normalized extends UriPathFormat

}

/** Uri encoding options */
object UriEncoding {

  sealed trait UriEncoding

  /** Use only default encoding */
  case object Default extends UriEncoding

  /** Perform additional encoding for [[Uri.userInfo]] */
  object UserInfo extends UriEncoding

  /** Perform additional encoding for [[Uri.path]] */
  object Segment extends UriEncoding

  /** Perform additional encoding for [[Uri.query]] */
  object Query extends UriEncoding

  /** Perform additional encoding for [[Uri.fragment]] */
  object Fragment extends UriEncoding

}

object UriSchemeDefaultPort {

  sealed case class SchemePort(port: Int, schemeHashCode: Int)

  val HTTP = SchemePort(80, "http".hashCode)
  val HTTPS = SchemePort(443, "https".hashCode)
  val FTP = SchemePort(21, "ftp".hashCode)

  def getSchemePort(scheme: String): Int = scheme.toLowerCase.hashCode match {
    case UriSchemeDefaultPort.HTTP.schemeHashCode => 80
    case UriSchemeDefaultPort.HTTPS.schemeHashCode => 443
    case UriSchemeDefaultPort.FTP.schemeHashCode => 21
    case _ => -1
  }
}

object Uri {


  def isValidCharInUri(ch: Char, level: UriEncoding): Boolean = {
    def queryEncoding: Boolean = ch match {
      case '/' | ':' | '~' | '$' | ',' | ';' | '|' => true
      case _ => segmentEncoding
    }

    def segmentEncoding: Boolean = ch match {
      case '@' | '^' => true
      case _ => false
    }

    ch match {
      case _ if (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))) || ((ch >= '0') && (ch <= '9')) => true
      case '\'' | '(' | ')' | '*' | '-' | '.' | '_' | '!' => true
      case _ => level match {
        case UriEncoding.Fragment => ch match {
          case '#' => true
          case _ => queryEncoding
        }
        case UriEncoding.Query => queryEncoding
        case UriEncoding.Segment => segmentEncoding
        case UriEncoding.UserInfo => ch match {
          case '&' | '=' => true
          case _ => false
        }
        case UriEncoding.Default => false
      }
    }
  }

  def renderParams(params: List[(String, Option[String])]): String = {
    val builder = new StringBuilder()
    Internals.renderParamsToBuilder(builder, params)
    builder.toString()
  }


  /** Uri encode a string
    *
    * @param text  Input text
    * @param level Encoding level
    * @return Encoded string
    */
  def encode(text: String, level: UriEncoding): String = Internals.encode(text, level, Internals.encodeByte)

  /** Double encode a string
    *
    * @param text  Input text
    * @param level Encoding level
    * @return Encoded string
    */
  def doubleEncode(text: String, level: UriEncoding): String = Internals.encode(text, level, Internals.doubleEncodeByte)

  /** Uri encode a string
    *
    * @param text Input text
    * @return Encoded string
    */
  def encode(text: String): String = encode(text, UriEncoding.Default)

  /** Double encode a string
    *
    * @param text Input text
    * @return Encoded string
    */
  def doubleEncode(text: String): String = doubleEncode(text, UriEncoding.Default)

  /** Encode a path segment
    *
    * @param text Input segment
    * @return Encoded segment
    */
  def encodeSegment(text: String): String = encode(text, UriEncoding.Segment)

  /** Double encode a path segment
    *
    * @param text Input segment
    * @return Encoded segment
    */
  def doubleEncodeSegment(text: String): String = doubleEncode(text, UriEncoding.Segment)

  /** Encode a query string
    *
    * @param text Input query
    * @return Encoded query
    */
  def encodeQuery(text: String): String = encode(text, UriEncoding.Query)

  /** Encode a fragment string
    *
    * @param text Input fragement
    * @return Encoded fragment
    */
  def encodeFragment(text: String): String = encode(text, UriEncoding.Fragment)

  /** Encode a Uri user info
    *
    * @param text Input user info
    * @return Encoded user info
    */
  def encodeUserInfo(text: String): String = encode(text, UriEncoding.UserInfo)

  def parseParamsAsPairs(query: String): List[(String, Option[String])] = {
    def parseParams(current: Int, keyIndex: Int, valueIndex: Int,
                    acc: List[(String, Option[String])]): List[(String, Option[String])] = {
      def getKey: String = if (keyIndex == -1) {
        ""
      } else if (valueIndex == -1) {
        UriParser.decodeString(query.substring(keyIndex, current))
      } else {
        UriParser.decodeString(query.substring(keyIndex, valueIndex - 1))
      }
      def getValue: Option[String] = if (valueIndex == -1) {
        None
      } else {
        Some(UriParser.decodeString(query.substring(valueIndex, current)))
      }
      if (current == query.length) {
        if (keyIndex > -1 || valueIndex > -1) {
          (getKey -> getValue) :: acc
        } else {
          acc
        }
      } else {
        query(current) match {
          case '=' => parseParams(current + 1, keyIndex, current + 1, acc)
          case '&' =>
            val key = getKey
            val value = getValue
            parseParams(current + 1, current + 1, -1, (key -> value) :: acc)
          case _ if keyIndex == -1 => parseParams(current + 1, current, valueIndex, acc)
          case _ => parseParams(current + 1, keyIndex, valueIndex, acc)
        }

      }
    }
    parseParams(0, -1, -1, Nil).reverse
  }

  def fromString(text: String): Option[Uri] = UriParser.tryParse(text)

  object Internals {

    val schemeRegex = """[a-zA-Z][\w\+\-\.]*""".r
    val hostRegex = """((\[[a-fA-F\d:\.]*\])|([\w\-\._~%!\$&'\(\)\*\+,;=]*))""".r
    val segmentRegex = """^/*[\w\-\._~%!\$&'\(\)\*\+,;=:@\^\|\[\]{}]*$""".r
    val ipRegex = """^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$""".r

    def isValidScheme(scheme: String) = schemeRegex.findFirstIn(scheme).nonEmpty

    def isValidHost(host: String) = hostRegex.findFirstIn(host).nonEmpty

    def isValidSegment(segment: String) = segmentRegex.findFirstMatchIn(segment).nonEmpty

    def intToHexByte(n: Int): Byte = Integer.toHexString(n)(0).toUpper.asInstanceOf[Byte]

    def isIp(host: String) = ipRegex.findFirstIn(host).nonEmpty

    def encode(text: String, level: UriEncoding, byteEncoder: (Byte, UriEncoding) => List[Byte]) = {
      if (text == null || text.isEmpty) {
        text
      } else {
        val original = text.getBytes(StandardCharsets.UTF_8)

        // count how many characters are affected by the encoding
        val (charsToReplace, charsToEncode) = original.foldLeft((0, 0)) {
          case ((r, e), ' ') => (r + 1, e)
          case ((r, e), b) if !isValidCharInUri(b.asInstanceOf[Char], level) => (r, e + 1)
          case (x, _) => x
        }

        // check if any characters are affected
        if (charsToReplace == 0 && charsToEncode == 0) {
          text
        } else {

          // copy, replace, and encode characters
          val encoded = original.flatMap { byte =>
            byte.asInstanceOf[Char] match {
              case ch if isValidCharInUri(ch, level) => List(byte)
              case ' ' => List[Byte](0x2b) // '+'
              case _ => List[Byte](
                0x25, // '%'
                Internals.intToHexByte((byte >> 4) & 15),
                Internals.intToHexByte(byte & 15))
            }
          }
          new String(encoded, StandardCharsets.US_ASCII)
        }
      }
    }

    def encodeByte(byte: Byte, level: UriEncoding): List[Byte] =
      byte.asInstanceOf[Char] match {
        case ch if isValidCharInUri(ch, level) => List(byte)
        case ' ' => List[Byte](0x2b) // '+'
        case _ => List[Byte](
          0x25, // '%'
          Internals.intToHexByte((byte >> 4) & 15),
          Internals.intToHexByte(byte & 15))
      }

    def doubleEncodeByte(byte: Byte, level: UriEncoding): List[Byte] =
      byte.asInstanceOf[Char] match {
        case ch if isValidCharInUri(ch, level) => List(byte)
        case ' ' => List[Byte](0x25, '2', 'b') // '%2b'
        case _ => List[Byte](
          0x25, // '%'
          '2',
          '5',
          Internals.intToHexByte((byte >> 4) & 15),
          Internals.intToHexByte(byte & 15))
      }

    def renderParamsToBuilder(builder: StringBuilder, params: List[(String, Option[String])]): Unit = {
      def addParam(q: List[(String, Option[String])], first: Boolean = true): Unit = q match {
        case Nil =>
        case (key, v) :: rest =>
          if (!first) {
            builder.append('&')
          }
          builder.append(Uri.encodeQuery(key))
          v.foreach { value =>
            builder.append('=')
            builder.append(Uri.encodeQuery(value))
          }
          addParam(rest, first = false)
      }
      addParam(params)
    }
  }

}

/** An immutable case class representation of a Uniform Resource Identifier.
  *
  * <p>It provides a fluent interface for derive other Uri's in addition to the regular case class `copy` mechanism.
  *
  * @param scheme        Uri Scheme.
  * @param host          Host.
  * @param port          Port.
  * @param user          User part of Uri Authority.
  * @param password      Password part of Uri Authority.
  * @param segments      Uri Path, represented by an ordered list of segments without their '/' separator.
  * @param params        Ordered list of query key/value pairs. An empty list represents the existence of a trailing '?'
  *                      without any parameters. Empty keys, e.g. '&&', '&=', or leading/trailing '&' are represented by empty
  *                      string, while empty values are [[None]].
  * @param fragment      Uri fragment.
  * @param trailingSlash true if the path has a trailing '/'.
  */
case class Uri(scheme: String, host: String = "", port: Int = -1, user: Option[String] = None, password: Option[String] = None, segments: List[String] = Nil, params: Option[List[(String, Option[String])]] = None, fragment: Option[String] = None, trailingSlash: Boolean = false) {

  // TODO: This should be extendable so that other schemes can provide their own default port
  lazy val usesDefaultPort: Boolean = if (port == -1) true // Is this what we want to do for -1 on all schemes?
  else
    UriSchemeDefaultPort.SchemePort(port, scheme.toLowerCase.hashCode) match {
      case UriSchemeDefaultPort.HTTP => true
      case UriSchemeDefaultPort.HTTPS => true
      case UriSchemeDefaultPort.FTP => true
      case _ => false
    }

  /** [[host]] [ + ":" + [[port]] ]
    *
    * @return
    */
  def hostPort: String = if (usesDefaultPort) host else s"$host:$port"

  /** [[scheme]] + "://" + [[host]] [ + ":" + [[port]] ]
    * unless [[port]] is the default port of the Uri's [[scheme]], in which case ":" + [[port]] is omitted.
    *
    * @return
    */
  def schemeHostPort: String = s"$scheme://$hostPort"

  /** [[scheme]] + "://" + [[host]] [ + ":" + [[port]] ] + [[path]]
    * unless [[port]] is the default port of the Uri's [[scheme]], in which case ":" + [[port]] is omitted.
    *
    * @return
    */
  def schemeHostPortPath: String = s"$schemeHostPort$path"

  /** [[path]] [ + "?" + [[query]] ] [ + "#" + [[fragment]] ]
    *
    * @return
    */
  def pathQueryFragment: String = {

    // TODO: All the string render accessors should have individual StringBuilder ones so they can be combined consistently
    // and efficiently
    val result = new StringBuilder()
    result.append(path)
    query.foreach { q =>
      result.append('?')
      result.append(q)
    }
    fragment.foreach { f =>
      result.append('#')
      result.append(Uri.encodeFragment(f))
    }
    result.toString
  }

  /** [ + "?" + [[query]] ] [ + "#" + [[fragment]] ]
    *
    * @return
    */
  def queryFragment: String = {
    val result = new StringBuilder()
    query.foreach { q =>
      result.append('?')
      result.append(q)
    }
    fragment.foreach { f =>
      result.append('#')
      result.append(Uri.encodeFragment(f))
    }
    result.toString
  }

  /** [ [[userInfo]] +] [[host]] [ + ":" + [[port]] ]
    * User and password are presented the same as in [[userInfo]], i.e. unencoded.
    *
    * @return
    */
  def authority: String = userInfo match {
    case None => hostPort
    case Some(info) => s"$info@$hostPort"
  }

  /** [[user + ":" + [[password]] ]
    * where either user or password can be [[None]] and are represented as empty string. However if both are [[None]],
    * the result of this accessor is also [[None]].
    *
    * @return
    */
  def userInfo: Option[String] = user match {
    case None => password match {
      case None => None
      case Some(p) => Some(s":$p")
    }
    case Some(u) => password match {
      case None => Some(u)
      case Some(p) => Some(s"$u:$p")
    }
  }

  /** Uri path.
    *
    * @return
    */
  def path: String = {
    val builder = new StringBuilder()
    segments.foreach { segment =>
      builder.append('/')
      builder.append(segment)
    }
    if (trailingSlash) builder.append('/')
    builder.toString()
  }

  /** Query string without the leading '?'.
    *
    * @return
    */
  def query: Option[String] = params.map(Uri.renderParams)

  /** true if [[host]] is an IP address */
  lazy val hostIsIp = Uri.Internals.isIp(host)

  /** true if [[path]] is either "http" or "https" */
  lazy val isHttpOrHttps =  List("http", "https").filter(_.equalsIgnoreCase(scheme))

  def pathStartsWith(segments: List[String]): Boolean =
    if(this.segments.length < segments.length) false
  else segments.zip(this.segments).exists { case (a, b) => a == b }

  /** Create a new Uri at a appended path.
    *
    * <p>An empty trailing segment indicates a trailing slash for the resulting Uri.
    *
    * <p>Throws NullPointerException on a non-trailing null segment.
    * <p>Throws IllegalArgumentException on a non-trailing empty segment.
    * <p>Throws IllegalArgumentException on a segment with illegal characters.
    *
    * @param segments Path segments.
    * @return New uri instance.
    */
  def at(segments: String*): Uri =
    if (segments.isEmpty) this
    else {
      def checkSegments(segments: List[String], acc: List[String]): (List[String], Boolean) = segments match {
        case h :: Nil if h == null || h.isEmpty => (acc.reverse, true)
        case h :: Nil => ((h :: acc).reverse, false)
        case h :: tail if h == null =>
          throw new NullPointerException("A segment was null")
        case h :: tail if h == null || h.isEmpty =>
          throw new IllegalArgumentException("A non-trailing segment was empty")
        case h :: tail if !Uri.Internals.isValidSegment(h) =>
          throw new IllegalArgumentException(s"Segment '$h' containts invalid characters")
        case h :: tail => checkSegments(tail, h :: acc)
      }

      val (newSegments, trailingSlash) = checkSegments(segments.toList, this.segments.reverse)
      copy(segments = newSegments, trailingSlash = trailingSlash)
    }

  /** Create a new Uri with appended path/query/fragment.
    *
    * <p>Returns the original Uri, if pathQueryFragment cannot be parsed
    * <p>Throws NullPointerException on null pathQueryFragment.
    * <p>Throws IllegalArgumentException on empty pathQueryFragment.
    *
    * @param pathQueryFragment Path/query/fragment string.
    * @return New Uri instance.
    */
  def atPath(pathQueryFragment: String): Uri = pathQueryFragment match {
    case x if x == null =>
      throw new NullPointerException("pathQueryFragment cannot be null")
    case x if x.isEmpty =>
      throw new IllegalArgumentException("pathQueryFragment cannot be empty")
    case _ =>
      val uriToParse = pathQueryFragment(0) match {
        case '/' => s"X://$pathQueryFragment"
        case _ => s"X:///$pathQueryFragment"
      }
      // TODO: run only the parts of UriParser needed for pathQueryFragment
      UriParser.tryParse(uriToParse) match {
        case None => this
        case Some(uri) =>
          Iff(this).orElse(uri.segments.nonEmpty) { x =>
            x.at(uri.segments: _*)
            val uri1 = at(uri.segments: _*)
            if (uri.trailingSlash) uri1.withTrailingSlash() else uri1.withoutTrailingSlash()
          }.orElse(uri.params.nonEmpty)(_.withParams(uri.params.get))
            .orElse(uri.fragment.nonEmpty)(_.withFragment(uri.fragment))
            .get
      }
  }

  /** Create a new Uri at a different absolute path.
    *
    * @param pathQueryFragment Path/query/fragment string.
    * @return New uri instance.
    */
  def atAbsolutePath(pathQueryFragment: String): Uri = withoutPathQueryFragment().atPath(pathQueryFragment)

  /** Create a new Uri based on the current instance with an additional query parameter.
    *
    * @param key   Query key.
    * @param value Query value. Value must exist.
    *              Use [[withParam(String)]] or [[withParam(String,Option[String])]] for value-less parameter.
    * @return New uri.
    */
  def withParam(key: String, value: String): Uri = withParams(List(key -> Some(value)))

  /** Create a new Uri based on the current instance with an additional query parameter.
    *
    * @param key   Query key.
    * @param value Query value.
    * @return New uri.
    */
  def withParam(key: String, value: Option[String]): Uri = withParams(List(key -> value))

  /** Create a new Uri based on the current instance with an additional query parameter without a value.
    *
    * @param key Query key.
    * @return New uri.
    */
  def withParam(key: String): Uri = withParams(List(key -> None))

  /** Create a new Uri based on the current instance with additional query parameters.
    *
    * @param params List of query key/value pairs.
    * @return New uri.
    */
  def withParams(params: List[(String, Option[String])]): Uri = (params, this.params) match {
    case (null, _) => this
    case (p1, None) => copy(params = Some(p1))
    case (p1, Some(p0)) => copy(params = Some(p0 ::: p1))
  }

  /** Create new Uri based on the current instance with parameters from another uri added.
    *
    * @param uri Other Uri.
    * @return New uri.
    */
  def withParamsFrom(uri: Uri): Uri = uri.params match {
    case None => this
    case Some(p) => withParams(p)
  }

  /** Create a new Uri based on the current instance with the provided querystring added.
    *
    * @param query Query string.
    * @return New uri.
    */
  def withQuery(query: String): Uri = withParams(Uri.parseParamsAsPairs(query))

  /** Create a copy of the current Uri with the Query removed.
    *
    * @return New uri.
    */
  def withoutQuery(): Uri = copy(params = None)

  /** Create a copy of the current Uri with the Query parameters removed.
    *
    * <p><b>Note:</b> same as [[withoutQuery()]].
    *
    * @return New uri.
    */
  def withoutParams(): Uri = withoutQuery()

  /** Create a copy of the current Uri with all occurences of a specific query parameter removed.
    *
    * @param key Query parameter key.
    * @return New uri.
    */
  def withoutParams(key: String): Uri = copy(params = params.map(_.filter(_._1 != key)))

  /** Create a new Uri based on the current instance with the given credentials.
    *
    * @param user     User.
    * @param password Password.
    * @return New uri.
    */
  def withCredentials(user: Option[String], password: Option[String]): Uri = copy(user = user, password = password)

  /** Create a new Uri based on the current instance with the credentials of another uri.
    *
    * @param uri Input uri.
    * @return New uri.
    */
  def withCredentialsFrom(uri: Uri): Uri = copy(user = uri.user, password = uri.password)

  /** Create a copy of the current Uri with credentials removed.
    *
    * <p><b>Note:</b> Same as .withCredentials(None,None)
    *
    * @return New uri.
    */
  def withoutCredentials(): Uri = copy(user = None, password = None)

  /** Create a new Uri based on the current instance with the given fragment.
    *
    * @param fragment Fragment. Use [[withFragment(String, Option[String])]] or [[withoutFragment()]] to remove fragment.
    * @return New uri.
    */
  def withFragment(fragment: String): Uri = copy(fragment = Some(fragment))

  /** Create a new Uri based on the current instance with the given fragment.
    *
    * @param fragment Fragment.
    * @return New uri.
    */
  def withFragment(fragment: Option[String]): Uri = copy(fragment = fragment)

  /** Create a copy of the current Uri with the fragment removed.
    *
    * @return New uri.
    */
  def withoutFragment(): Uri = copy(fragment = None)

  /** Create a new Uri based on the current instance with only a subset of the original path.
    *
    * @param count Number of segments to keep. If count is larger than number of segments, Uri remains
    *              unchanged. If count is 0, the path is removed.
    *              <p>Throws [[IllegalArgumentException]] on negative count
    * @return New uri.
    */
  def withFirstSegments(count: Int): Uri = count match {
    case c if c < 0 => throw new IllegalArgumentException(s"count cannot be negative")
    case c if c >= segments.length => this
    case c if c == 0 => withoutPath()
    case c => copy(segments = segments.take(c))
  }

  /** Create a new Uri based on the current instance with only a subset of the original path.
    *
    * @param count Number of segments to drop. If count is larger than number of segments, the path is removed.
    *              If count is 0, the Uri remains unchanged.
    *              <p>Throws [[IllegalArgumentException]] on negative count
    * @return New uri.
    */
  def withoutFirstSegments(count: Int): Uri = count match {
    case c if c < 0 => throw new IllegalArgumentException(s"count cannot be negative")
    case c if c == 0 || 0 == segments.length => this
    case c if c >= segments.length => withoutPath()
    case c => copy(segments = segments.takeRight(segments.length - c))
  }

  /** Create a copy of the current Uri with the last segment removed.
    *
    * @return New uri.
    */
  def withoutLastSegment(): Uri = withoutLastSegments(1)

  /** Create a new Uri based on the current instance with only a subset of the original path.
    *
    * @param count Number of segments to remove from end of path. If count is larger than number of segments, the
    *              path is removed. If count is 0, the Uri remains unchanged.
    *              <p>Throws [[IllegalArgumentException]] on negative count
    * @return New uri.
    */
  def withoutLastSegments(count: Int): Uri = count match {
    case c if c < 0 => throw new IllegalArgumentException(s"count cannot be negative")
    case _ => withFirstSegments(Math.max(0, segments.length - count))
  }

  /** Create a new Uri with the current [[path]] removed.
    *
    * @return New uri.
    */
  def withoutPath(): Uri = copy(segments = Nil)

  /** Create a copy of the current Uri with the [[path]], [[query]] and [[fragment]] removed.
    *
    * @return New uri.
    */
  def withoutPathQueryFragment(): Uri = copy(segments = Nil, params = None, fragment = None)

  /** Create a copy of the current Uri with the [[userInfo]], [[path]], [[query]] and [[fragment]] removed.
    *
    * @return New uri.
    */
  def withoutCredentialsPathQueryFragment(): Uri = copy(
    user = None,
    password = None,
    segments = Nil,
    params = None,
    fragment = None)

  /** Create a new Uri based on the current instance with a trailing slash.
    *
    * @return New uri.
    */
  def withTrailingSlash(): Uri = copy(trailingSlash = true)

  /** Create a copy of the current Uri with the trailing slash removed.
    *
    * @return New uri.
    */
  def withoutTrailingSlash(): Uri = copy(trailingSlash = false)

  /** Create a new Uri based on the current instance with a different scheme.
    *
    * @param scheme New scheme.
    * @return New uri.
    */
  def withScheme(scheme: String): Uri = if (Uri.Internals.isValidScheme(scheme)) {
    val port = if (usesDefaultPort) {
      UriSchemeDefaultPort.getSchemePort(scheme)
    } else {
      this.port
    }
    copy(scheme = scheme, port = port)
  } else {
    throw new IllegalArgumentException(s"invalid scheme: $scheme")
  }

  /** Create a new Uri based on the current instance with a different host.
    *
    * @param host New host.
    * @return New uri.
    */
  def withHost(host: String): Uri = if (Uri.Internals.isValidHost(host)) {
    copy(host = host)
  } else {
    throw new IllegalArgumentException(s"invalid host: $scheme")
  }

  /** Create a new Uri based on the current instance with a different port.
    *
    * @param port New port.
    * @return New uri.
    */
  def withPort(port: Int): Uri = if (port < 0 || port > UriParser.MAX_PORT) {
    throw new IllegalArgumentException(s"invalid port: $port")
  } else {
    UriParser.determinePort(copy(port = port))
  }

  /** Render the Uri as standard string representation.
    *
    * @return
    */
  // Note: Not overriding toString for this, because it hides the case class string serialization
  def toUriString: String = toUriString(true)

  /** Render the Uri as standard string representation.
    *
    * @param includePassword If false, any occurence of [[password]] is replaced with 'xxx'. Useful when writing Uris
    *                        to logs.
    * @return
    */
  def toUriString(includePassword: Boolean = true) = {
    val result = new StringBuilder()
    result.append(scheme)
    result.append("://")

    // add user and password information
    user.foreach { u =>
      result.append(Uri.encodeUserInfo(u))
      password.foreach { p =>
        result.append(":")
        result.append(if (includePassword) Uri.encodeUserInfo(p) else "xxx")
      }
      result.append("@")
    }

    // add domain
    result.append(host)

    // add port
    if (!usesDefaultPort) {
      result.append(":")
      result.append(port)
    }

    // add path
    segments.foreach { segment =>
      result.append('/')
      result.append(segment)
    }
    if (trailingSlash) {
      result.append('/')
    }

    // add query
    params.foreach { p =>
      result.append('?')
      Uri.Internals.renderParamsToBuilder(result, p)
    }

    // add fragment
    fragment.foreach { f =>
      result.append('#')
      result.append(Uri.encodeFragment(f))
    }
    result.toString
  }
}
