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
  case object UserInfo extends UriEncoding

  /** Perform additional encoding for [[Uri.path]] */
  case object Segment extends UriEncoding

  /** Perform additional encoding for [[Uri.query]] */
  case object Query extends UriEncoding

  /** Perform additional encoding for [[Uri.fragment]] */
  case object Fragment extends UriEncoding

}

object Uri {

  def intToHexByte(n: Int): Byte = Integer.toHexString(n)(0).toUpper.asInstanceOf[Byte]


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

  /** Uri encode a string
    *
    * @param text  Input text
    * @param level Encoding level
    * @return Encoded string
    */
  def encode(text: String, level: UriEncoding): String = {
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
            case ch@' ' => List[Byte](0x2b) // '+'
            case _ => List[Byte](
              0x25, // '%'
              intToHexByte((byte >> 4) & 15),
              intToHexByte(byte & 15))
          }
        }
        new String(encoded, StandardCharsets.US_ASCII)
      }
    }
  }


  /// <summary>
  /// Double encode a string.
  /// </summary>
  /// <param name="text">Input text.</param>
  /// <param name="level">Encoding level.</param>
  /// <returns>Encoded string.</returns>
  def doubleEncode(text: String, level: UriEncoding): String = ???

  //  {
  //    if(string.IsNullOrEmpty(text)) {
  //      return text;
  //    }
  //    var original = Encoding.UTF8.GetBytes(text);
  //
  //    // count how many characters are affected by the encoding
  //    var charsToReplace = 0;
  //    var charsToEncode = 0;
  //    var length = original.Length;
  //    for(var i = 0; i < length; i++) {
  //      var ch = (char)original[i];
  //      if(ch == ' ') {
  //        charsToReplace++;
  //      } else if(!IsValidCharInUri(ch, level)) {
  //        charsToEncode++;
  //      }
  //    }
  //
  //    // check if any characters are affected
  //    if((charsToReplace == 0) && (charsToEncode == 0)) {
  //      return text;
  //    }
  //
  //    // copy, replace, and encode characters
  //    var encoded = new byte[length + (charsToReplace * 2) + (charsToEncode * 4)];
  //    var index = 0;
  //    for(var j = 0; j < length; j++) {
  //      var asciiByte = original[j];
  //      var asciiChar = (char)asciiByte;
  //      if(IsValidCharInUri(asciiChar, level)) {
  //        encoded[index++] = asciiByte;
  //      } else if(asciiChar == ' ') {
  //
  //        // replace ' ' with '%2b'
  //        encoded[index++] = 0x25; // '%'
  //        encoded[index++] = (byte)'2';
  //        encoded[index++] = (byte)'b';
  //      } else {
  //
  //        // replace char with '%25' + code
  //        encoded[index++] = 0x25; // '%'
  //        encoded[index++] = (byte)'2';
  //        encoded[index++] = (byte)'5';
  //        encoded[index++] = (byte)StringUtil.IntToHexChar((asciiByte >> 4) & 15);
  //        encoded[index++] = (byte)StringUtil.IntToHexChar(asciiByte & 15);
  //      }
  //    }
  //    return Encoding.ASCII.GetString(encoded);
  //  }

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
}

case class Uri(scheme: String,
               user: Option[String] = None,
               password: Option[String] = None,
               hostname: String = "",
               port: Int = -1,
               segments: List[String] = Nil,
               params: Option[List[(String, Option[String])]] = None,
               fragment: Option[String] = None,
               usesDefaultPort: Boolean = true,
               trailingSlash: Boolean = false) {

  def userInfo: String = ???

  def path: String = ???

  def query: String = ???

  // Not overriding toString, because it hides the case class string serialization
  def asString: String = asString(true)

  def asString(includePassword: Boolean = true) = {
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
    result.append(hostname)

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
    def addParam(q: List[(String, Option[String])], first: Boolean = true): Unit = q match {
      case Nil =>
      case (key, v) :: rest =>
        if (!first) {
          result.append('&')
        }
        result.append(Uri.encodeQuery(key))
        v.foreach { value =>
          result.append('=')
          result.append(Uri.encodeQuery(value))
        }
        addParam(rest, first = false)
    }
    params.foreach { p =>
      result.append('?')
      addParam(p, first = true)
    }

    // add fragment
    fragment.foreach { f =>
      result.append('#')
      result.append(Uri.encodeFragment(f))
    }
    result.toString
  }
}
