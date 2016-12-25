package plug


import java.nio.charset.StandardCharsets

import plug.Steps.{NextStep, RecursiveStep}

object Steps {

  sealed trait NextStep

  sealed trait RecursiveStep extends NextStep

  case object End extends NextStep

  case object Error extends NextStep

  case object Path extends RecursiveStep

  case object PathBackslash extends RecursiveStep

  case object Query extends RecursiveStep

  case object Fragment extends RecursiveStep

  val END_OF_STRING = '\uFFFF'

  def determineStep(c: Char) = c match {
    case END_OF_STRING => End
    case '/' => Path
    case '\\' => PathBackslash
    case '?' => Query
    case '#' => Fragment
  }
}

object UriParser {
  // NOTE (steveb): UriParser parses absolute URIs based on RFC3986 (http://www.ietf.org/rfc/rfc3986.txt), with
  //                the addition of ^, |, [, ], { and } as a valid character in segments, queries, and fragments;
  //                and \ as valid segment separator.

  val HTTP_HASHCODE = "http".hashCode
  val HTTPS_HASHCODE = "https".hashCode
  val FTP_HASHCODE = "ftp".hashCode

  case class StepResult(current: Int,
                        nextStep: NextStep,
                        parts: Uri)

  //--- Class Methods ---
  def tryParse(text: String): Option[Uri] = {
    if (text == null || text.isEmpty) {
      None
    } else {
      val length = text.length
      tryParseScheme(text, length, 0) match {
        case None => None
        case Some((schemePosition, scheme)) => tryParseAuthority(text, length, schemePosition, Uri(scheme)) match {
          case None => None
          case Some(authorityResult) =>
            val portResult = authorityResult.copy(
              parts = determinePort(authorityResult.parts)
            )
            def handleStep(stepResult: StepResult): StepResult = stepResult.nextStep match {
              case (Steps.End | Steps.Error) => stepResult
              case x: RecursiveStep =>
                val y = x match {
                  case (Steps.Path | Steps.PathBackslash) => tryParsePath(text, length, stepResult.current, stepResult.parts)
                  case Steps.Query => tryParseQuery(text, length, stepResult.current, stepResult.parts)
                  case Steps.Fragment => tryParseFragment(text, length, stepResult.current, stepResult.parts)
                }
                handleStep(y)
            }
            handleStep(portResult) match {
              case StepResult(_, Steps.End, parts) => Some(parts)
              case _ => None
            }
        }
      }
    }
  }

  def tryParseScheme(text: String, length: Int, start: Int): Option[(Int, String)] = {
    def parseScheme(current: Int): Option[(Int, String)] = {
      if (current >= length) {
        None
      } else {
        val c = text(current)
        if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || ((c >= '0') && (c <= '9'))) {

          // valid character, keep parsing
          parseScheme(current + 1)
        } else if (c == ':') {
          if ((length - current >= 3) && text.substring(current + 1, current + 3).equals("//")) {

            // found "://" sequence at current location, we're done with scheme parsing
            Some((current + 3, text.substring(start, current)))
          } else {
            None
          }
        } else {
          None
        }
      }
    }
    val c = text(start)
    if (!(((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))) {

      // scheme must begin with alpha character
      None
    } else {
      parseScheme(start + 1)
    }
  }

  def tryParseAuthority(text: String, length: Int, current1: Int, parts: Uri): Option[StepResult] = {
    sealed trait Branch
    case object HostnameOrUserInfo extends Branch
    case object HostnameOrUserInfoAfterColon extends Branch
    case object HostnameOrIPv6Address extends Branch
    case object PortNumber extends Branch
    case object IPv6 extends Branch

    // check first character; it could tell us if we're parsing an IPv6 address
    val (c, decode, ipv6) = if (current1 < length) {
      text(current1) match {
        case c1@('%' | '+') => (c1, true, false)
        case c1@'[' => (c1, false, true)
        case c1 => (c1, false, false)
      }
    } else {
      (Steps.END_OF_STRING, false, false)
    }

    def parseAuthority(c: Char, current: Int, last: Int, decode: Boolean, branch: Branch, hostnameOrUsername: String, parts: Uri): Option[StepResult] = {

      def nextChar(allowIPv6: Boolean = false): (Char, Int, Boolean, Boolean) = {
        val next = current + 1
        if (next < length) {
          text(next) match {
            case c1@('%' | '+') => (c1, next, true, false)
            case c1@'[' if allowIPv6 => (c1, next, decode, true)
            case c1 => (c1, next, decode, false)
          }
        } else {
          (Steps.END_OF_STRING, next, decode, false)
        }
      }

      def iffDecode(content: String) = if (decode) decodeString(content) else content

      branch match {
        case HostnameOrUserInfo =>
          //// parse hostname -OR- user-info
          if (((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')) ||
            ((c >= '$') && (c <= '.')) || // one of: $%&'()*+,-.
            (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') ||
            Character.isLetterOrDigit(c)) {

            // valid character, keep parsing
            val (c1, current1, decode1, _) = nextChar()
            parseAuthority(c1, current1, last, decode1, branch, null, parts)
          } else if (c == ':') {

            // part before ':' is either a username or hostname
            val hostnameOrUsername = text.substring(last, current)
            parseAuthority(c, current, current + 1, decode, HostnameOrUserInfoAfterColon, hostnameOrUsername, parts)
          } else if (c == '@') {

            // part before '@' must be username since we didn't find ':'
            val user = iffDecode(text.substring(last, current))
            parseAuthority(c, current, current + 1, decode = false, HostnameOrIPv6Address, null, parts.copy(user = Some(user)))
          } else if ((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == Steps.END_OF_STRING)) {

            // part before '/', '\', '?', '#' must be hostname
            if (decode) {

              // hostname cannot contain encoded characters
              None
            } else {
              val hostname = text.substring(last, current)
              Some(StepResult(current + 1, Steps.determineStep(c), parts.copy(host = hostname)))
            }
          } else {
            None
          }

        case HostnameOrUserInfoAfterColon =>
          //// parse hostname -OR- user-info AFTER we're parsed a colon (':')
          val (c1, current1, decode1, _) = nextChar()

          if (((c1 >= 'a') && (c1 <= 'z')) ||
            ((c1 >= 'A') && (c1 <= 'Z')) ||
            ((c1 >= '0') && (c1 <= '9')) ||
            ((c1 >= '$') && (c1 <= '.')) || // one of: $%&'()*+,-.
            (c1 == '!') || (c1 == ';') || (c1 == '=') ||
            (c1 == '_') || (c1 == '~') ||
            Character.isLetterOrDigit(c1)) {

            // valid character, keep parsing
            parseAuthority(c1, current1, last, decode1, branch, hostnameOrUsername, parts)
          } else if (c1 == '@') {

            // part before ':' was username
            val user = iffDecode(hostnameOrUsername)

            // part after ':' is password
            val password = iffDecode(text.substring(last, current1))
            parseAuthority(c1, current1, current1 + 1, decode = false, HostnameOrIPv6Address, null, parts.copy(user = Some(user), password = Some(password)))
          } else if ((c1 == '/') || (c1 == '\\') || (c1 == '?') || (c1 == '#') || (c1 == Steps.END_OF_STRING)) {

            // part before ':' was hostname
            if (decode) {

              // hostname cannot contain encoded characters
              None
            } else {
              val hostname = hostnameOrUsername

              // part after ':' is port, parse and validate it
              tryParsePort(text, last, current1) match {
                case -1 => None
                case port => Some(StepResult(current1 + 1, Steps.determineStep(c1), parts.copy(host = hostname, port = port)))
              }
            }
          } else {
            None
          }
        case HostnameOrIPv6Address =>
          val (c1, current1, decode1, ipv6) = nextChar(allowIPv6 = true)
          if (ipv6) {
            // NOTE: we want to include the leading character in the final result, so last becomes current
            parseAuthority(c1, current1, current1, decode1, IPv6, null, parts)
          } else {
            if (((c1 >= 'a') && (c1 <= 'z')) ||
              ((c1 >= 'A') && (c1 <= 'Z')) ||
              ((c1 >= '0') && (c1 <= '9')) ||
              ((c1 >= '$') && (c1 <= '.')) || // one of: $%&'()*+,-.
              (c1 == '!') || (c1 == ';') || (c1 == '=') ||
              (c1 == '_') || (c1 == '~') ||
              Character.isLetterOrDigit(c1)) {

              // valid character, keep parsing
              val (c2, current2, decode2, _) = nextChar()
              parseAuthority(c2, current2, last, decode2, branch, null, parts)
            } else if (c1 == ':') {
              if (decode) {

                // hostname cannot contain encoded characters
                None
              } else {
                val hostname = text.substring(last, current1)
                parseAuthority(c1, current1, current1 + 1, decode1, PortNumber, null, parts.copy(host = hostname))
              }
            } else if ((c1 == '/') || (c1 == '\\') || (c1 == '?') || (c1 == '#') || (c1 == Steps.END_OF_STRING)) {
              if (decode) {

                // hostname cannot contain encoded characters
                None
              } else {
                val hostname = text.substring(last, current1)
                Some(StepResult(current1 + 1, Steps.determineStep(c1), parts.copy(host = hostname)))
              }
            } else {
              None
            }
          }
        case PortNumber =>
          val current1 = current + 1
          val c1 = if (current1 < length) text(current1) else Steps.END_OF_STRING
          if ((c1 >= '0') && (c1 <= '9')) {

            // valid character, keep parsing
            parseAuthority(c1, current1, last, decode, branch, null, parts)
          } else if ((c1 == '/') || (c1 == '\\') || (c1 == '?') || (c1 == '#') || (c1 == Steps.END_OF_STRING)) {
            tryParsePort(text, last, current1) match {
              case -1 => None
              case port => Some(StepResult(current1 + 1, Steps.determineStep(c1), parts.copy(port = port)))
            }
          } else {
            None
          }
        case IPv6 =>
          val current1 = current + 1
          val c1 = if (current1 < length) text(current1) else Steps.END_OF_STRING

          if (((c1 >= 'a') && (c1 <= 'f')) ||
            ((c1 >= 'A') && (c1 <= 'F')) ||
            ((c1 >= '0') && (c1 <= '9')) ||
            (c1 == ':') || (c1 == '.')) {

            // valid character, keep parsing
            parseAuthority(c1, current1, last, decode, branch, null, parts)
          } else if (c1 == ']') {
            val hostname = text.substring(last, current1 + 1)

            // check next character to determine correct state to transition to
            val current2 = current1 + 1
            val c2 = if (current2 < length) text(current2) else Steps.END_OF_STRING
            if (c2 == ':') {
              parseAuthority(c2, current2, current2 + 1, decode, PortNumber, null, parts.copy(host = hostname))
            } else if ((c2 == '/') || (c2 == '\\') || (c2 == '?') || (c2 == '#') || (c2 == Steps.END_OF_STRING)) {
              Some(StepResult(current2 + 1, Steps.determineStep(c2), parts.copy(host = hostname)))
            } else {
              None
            }
          } else {
            None
          }
      }
    }
    val branch = if (ipv6) IPv6 else HostnameOrUserInfo
    parseAuthority(c, current1, current1, decode, branch, null, parts)
  }

  def tryParsePath(text: String, length: Int, start: Int, parts: Uri): StepResult = {
    def parsePath(current: Int, last: Int,
                  hasLeadingBackslashes: Boolean, leading: Boolean = true,
                  parts: Uri): StepResult = {

      def next = current + 1

      def getSegment: (String, Boolean) = {
        val segment = text.substring(last, current)
        if (hasLeadingBackslashes) {
          (segment.replace('\\', '/'), false)
        } else (segment, hasLeadingBackslashes)
      }

      val c = if (current < length) text(current) else Steps.END_OF_STRING
      if ((c == '/') || (c == '\\')) {
        if (leading) {
          parsePath(next, last, hasLeadingBackslashes || (c == '\\'), leading, parts)
        } else {
          val (segment, hasLeadingBackslashes1) = getSegment
          parsePath(next, current + 1, hasLeadingBackslashes1, leading = true, parts.copy(segments = segment :: parts.segments))
        }
      } else if (
        ((c >= 'a') && (c <= '~')) || // one of: abcdefghijklmnopqrstuvwxyz{|}~
          ((c >= '@') && (c <= '_')) || // one of: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
          ((c >= '$') && (c <= ';')) || // one of: $%&'()*+,-./0123456789:;
          (c == '=') || (c == '!') ||
          Character.isLetterOrDigit(c)) {

        // no longer accept leading '/' or '\' characters
        parsePath(next, last, hasLeadingBackslashes, leading = false, parts)
      } else if ((c == '?') || (c == '#') || (c == Steps.END_OF_STRING)) {
        val (segments, trailingSlash) = if (last == current) {
          //trailingSlash = true;
          (parts.segments, true)
        } else {
          val (segment, _) = getSegment
          (segment :: parts.segments, false)
        }
        StepResult(current + 1, Steps.determineStep(c), parts.copy(segments = segments.reverse, trailingSlash = trailingSlash))
      } else {
        StepResult(current, Steps.Error, parts)
      }
    }
    parsePath(start, start, hasLeadingBackslashes = false, leading = true, parts)
  }

  def tryParseQuery(text: String, length: Int, start: Int, parts: Uri): StepResult = {
    def parseQuery(current: Int, last: Int, paramsKey: String, decode0: Boolean, parsingKey: Boolean, parts: Uri): StepResult = {
      def next = current + 1

      val (c, decode) = if (current < length) {
        text(current) match {
          case c1@('%' | '+') => (c1, true)
          case c1 => (c1, decode0)
        }
      } else {

        // use '\uFFFF' as end-of-string marker
        (Steps.END_OF_STRING, decode0)
      }

      def iffDecode(content: String) = if (decode) decodeString(content) else content

      if (
        ((c >= 'a') && (c <= '~')) || // one of: abcdefghijklmnopqrstuvwxyz{|}~
          ((c >= '?') && (c <= '_')) || // one of: ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
          ((c >= '\'') && (c <= ';')) || // one of: '()*+,-./0123456789:;
          (c == '$') || (c == '%') || (c == '!') ||
          Character.isLetterOrDigit(c)
      ) {

        // valid character, keep parsing
        parseQuery(next, last, paramsKey, decode, parsingKey, parts)
      } else if ((c == '&') || (c == '#') || (c == Steps.END_OF_STRING)) {
        val (parts1, parsingKey1) = if (parsingKey) {
          val parts1 = if (current != last) {

            // add non-empty key with empty value
            val (paramsKey) = iffDecode(text.substring(last, current))
            parts.copy(params = parts.params.map((paramsKey -> None) :: _))
          } else if (c == '&') {

            // this occurs in the degenerate case of two consecutive ampersands (e.g. "&&")
            parts.copy(params = parts.params.map(("" -> None) :: _))
          } else {
            parts
          }
          (parts1, parsingKey)
        } else {

          // add key with value
          val paramsValue = iffDecode(text.substring(last, current))
          (parts.copy(params = parts.params.map((paramsKey -> Some(paramsValue)) :: _)), true)
        }

        // check if we found a query parameter separator
        if (c == '&') {
          parseQuery(next, current + 1, paramsKey, decode0 = false, parsingKey = parsingKey1, parts1)
        } else {

          // we're done parsing the query string
          StepResult(next, Steps.determineStep(c), parts1.copy(params = parts1.params.map(_.reverse)))
        }
      } else if (c == '=') {
        if (parsingKey) {
          val paramsKey1 = iffDecode(text.substring(last, current))
          parseQuery(next, current + 1, paramsKey1, decode0 = false, parsingKey = false, parts)
        } else {
          parseQuery(next, last, paramsKey, decode, parsingKey, parts)
        }
      } else {
        StepResult(current, Steps.Error, parts)
      }
    }

    // by virtue of being here, we know there is some kind of query, so we initialize params to Some(Nil)
    parseQuery(start, start, null, decode0 = false, parsingKey = true, parts.copy(params = Some(Nil)))
  }

  def tryParseFragment(text: String, length: Int, start: Int, parts: Uri): StepResult = {
    def parseFragment(current: Int, decode: Boolean): StepResult = {
      //var decode = false;
      if (current < length) {
        val c = text(current)
        val decode2 = c match {
          case '%' => true
          case '+' => true
          case _ => decode
        }
        if (isFragmentChar(c)) {
          parseFragment(current + 1, decode2)
        } else {
          StepResult(current, Steps.Error, parts)
        }
      } else {
        val fragment = text.substring(start, current)
        val fragment2 = if (decode) {
          decodeString(fragment)
        } else fragment
        StepResult(current, Steps.End, parts.copy(fragment = Some(fragment2)))
      }
    }
    parseFragment(start, decode = false)
  }

  def isFragmentChar(c: Char): Boolean = {

    // ! 33

    // # 35 (not valid in path and query)
    // $ 36
    // % 37
    // & 38
    // ' 39
    // ( 40
    // ) 41
    // * 42
    // + 43
    // , 44
    // - 45
    // . 46
    // / 47
    // 0..9 48..57
    // : 58
    // ; 59

    // = 61

    // ? 63 (not valid in path)
    // @ 64
    // A..Z 65..90
    // [ 91
    // \ 92 (new)
    // ] 93
    // ^ 94
    // _ 95

    // a..z 97..122
    // { 123
    // | 124
    // } 125
    // ~ 126

    ((c >= 'a') && (c <= '~')) || // one of: abcdefghijklmnopqrstuvwxyz{|}~
      ((c >= '?') && (c <= '_')) || // one of: ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
      ((c >= '#') && (c <= ';')) || // one of: #$%&'()*+,-./0123456789:;
      (c == '=') ||
      (c == '!') ||
      Character.isLetterOrDigit(c)
  }

  def decodeString(text: String): String = {
    val length = text.length
    def decode2(textIndex: Int, bytes: List[Byte]): List[Byte] = {
      if (textIndex >= length) {
        bytes
      } else {
        val c = text(textIndex)
        c match {
          // + => space
          case '+' => decode2(textIndex + 1, ' '.toByte :: bytes)
          // % followed
          case '%' =>
            // if(((textIndex + 2) < length) && ((next = text[textIndex + 1]) != '%')) {
            if (textIndex + 2 < length) {
              text(textIndex + 1) match {
                case '%' => decode2(textIndex + 1, c.toByte :: bytes)
                case 'u' =>
                  if (textIndex + 5 < length) {
                    getChar(text, textIndex + 2, 4) match {
                      case -1 =>
                        // fall through to default char handler
                        decode2(textIndex + 1, c.toByte :: bytes)
                      case xchar =>
                        val charBytes = xchar.toChar.toString.getBytes(StandardCharsets.UTF_8)
                        decode2(textIndex + 6, charBytes.reverse.toList ::: bytes)
                    }
                  } else {
                    // fall through to default char handler
                    decode2(textIndex + 2, c.toByte :: bytes)
                  }
                case _ =>
                  getChar(text, textIndex + 1, 2) match {
                    case -1 =>
                      decode2(textIndex + 1, c.toByte :: bytes)
                    case xchar =>
                      val charByte = xchar.asInstanceOf[Byte]
                      decode2(textIndex + 3, charByte :: bytes)
                  }
              }
            } else {
              decode2(textIndex + 1, c.toByte :: bytes)
            }
          case _ => decode2(textIndex + 1, c.toByte :: bytes)
        }
      }
    }
    val bytes = decode2(0, Nil).reverse.toArray
    new String(bytes, StandardCharsets.UTF_8)
  }

  def determinePort(uriParts: Uri): Uri = uriParts.port match {
    case -1 =>
      val port = uriParts.scheme.toLowerCase.hashCode match {
        case HTTP_HASHCODE => 80
        case HTTPS_HASHCODE => 443
        case FTP_HASHCODE => 21
        case _ => -1
      }
      uriParts.copy(port = port, usesDefaultPort = true)
    case port if uriParts.usesDefaultPort => uriParts.copy(usesDefaultPort = false)
    case _ => uriParts
  }

  def getChar(text: String, start: Int, length: Int): Int = {
    def getChar2(offset: Int, result: Int): Int = {
      if (start + length <= offset) {
        result
      } else {
        val value = text(offset) match {
          case c if c >= '0' && c <= '9' => c - '0'
          case c if c >= 'a' && c <= 'f' => c - 'a' + 10
          case c if c >= 'A' && c <= 'F' => c - 'A' + 10
          case _ => -1
        }
        if (value == -1) value else getChar2(offset + 1, (result << 4) + value)
      }
    }
    getChar2(start, 0)
  }

  def tryParsePort(text: String, start: Int, end: Int): Int = {
    def parsePort(current: Int, value: Int): Int = text(current) match {
      case c if c >= '0' && c <= '9' =>
        val v = (c - '0') + value * 10
        // We don't have to check for negative since the only way we can get there is by overflow
        // and we kick out once we exceed 65535 already
        if (v > 65535) -1 else if (current + 1 == end) v else parsePort(current + 1, v)
      case _ => -1
    }
    if (start >= end) -1 else parsePort(start, 0)
  }

}
