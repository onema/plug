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
  // NOTE (steveb): XUriParser parses absolute URIs based on RFC3986 (http://www.ietf.org/rfc/rfc3986.txt), with
  //                the addition of ^, |, [, ], { and } as a valid character in segments, queries, and fragments;
  //                and \ as valid segment separator.

  //--- Constants ---


  //--- Class Fields ---
  val HTTP_HASHCODE = "http".hashCode
  val HTTPS_HASHCODE = "https".hashCode
  val LOCAL_HASHCODE = "local".hashCode
  val FTP_HASHCODE = "ftp".hashCode

  case class StepResult(current: Int,
                        nextStep: NextStep,
                        parts: Uri)

  //--- Class Methods ---
  def TryParse(text: String): Option[Uri] = {
    if (text == null || text.isEmpty) {
      None
    } else {
      val length = text.length
      TryParseScheme(text, length, 0) match {
        case None => None
        case Some((schemePosition, scheme)) => TryParseAuthority(text, length, schemePosition, Uri(scheme)) match {
          case None => None
          case Some(authorityResult) =>
            val portResult = authorityResult.copy(
              parts = DeterminePort(authorityResult.parts)
            )
            def handleStep(stepResult: StepResult): StepResult = stepResult.nextStep match {
              case (Steps.End | Steps.Error) => stepResult
              case x: RecursiveStep =>
                val y = x match {
                  case (Steps.Path | Steps.PathBackslash) => TryParsePath(text, length, stepResult.current, stepResult.parts)
                  case Steps.Query => TryParseQuery(text, length, stepResult.current, stepResult.parts)
                  case Steps.Fragment => TryParseFragment(text, length, stepResult.current, stepResult.parts)
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

  def TryParseScheme(text: String, length: Int, start: Int): Option[(Int, String)] = {
    def ParseScheme(current: Int): Option[(Int, String)] = {
      if (current >= length) {
        None
      } else {
        val c = text(current)
        if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || ((c >= '0') && (c <= '9'))) {

          // valid character, keep parsing
          ParseScheme(current + 1)
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
      ParseScheme(start + 1)
    }
  }

  def TryParseAuthority(text: String, length: Int, current1: Int, parts: Uri): Option[StepResult] = {
    //      var last = current1
    //      var current = current1
    ////      user = null;
    ////      password = null;
    ////      hostname = null;
    ////      port = -1;
    //
    //      // check first character; it could tell us if we're parsing an IPv6 address
    val (c, decode, ipv6) = if(current1 < length) {
      text(current1) match {
        case c1@('%' | '+') => (c1, true, false)
        case c1@'[' => (c1, false, true)
      }
    } else {
      (Steps.END_OF_STRING, false, false)
    }
    if(ipv6) {
      TryParseIPv6(text, length, current1, parts)
    } else {
      None
    }
    //// parse hostname -OR- user-info
    //string hostnameOrUsername;
    //for(;;) {
    //  if(
    //    ((c >= 'a') && (c <= 'z')) ||
    //      ((c >= 'A') && (c <= 'Z')) ||
    //      ((c >= '0') && (c <= '9')) ||
    //      ((c >= '$') && (c <= '.')) ||   // one of: $%&'()*+,-.
    //      (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') ||
    //      char.IsLetterOrDigit(c)
    //  ) {
    //
    //    // valid character, keep parsing
    //  } else if(c == ':') {
    //
    //    // part before ':' is either a username or hostname
    //    hostnameOrUsername = text.Substring(last, current - last);
    //    last = current + 1;
    //    goto hostnameOrUserInfoAfterColon;
    //  } else if(c == '@') {
    //
    //    // part before '@' must be username since we didn't find ':'
    //    user = text.Substring(last, current - last);
    //    if(decode) {
    //      user = Decode(user);
    //      decode = false;
    //    }
    //    last = current + 1;
    //    goto hostnameOrIPv6Address;
    //  } else if((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //
    //    // part before '/', '\', '?', '#' must be hostname
    //    if(decode) {
    //
    //      // hostname cannot contain encoded characters
    //      return -1;
    //    }
    //    hostname = text.Substring(last, current - last);
    //    next = (nextStep)c;
    //    return current + 1;
    //  } else {
    //    return -1;
    //  }
    //
    //  // continue on by reading the next character
    //  ++current;
    //  if(current < length) {
    //    c = text[current];
    //    switch(c) {
    //      case '%':
    //      case '+':
    //      decode = true;
    //      break;
    //    }
    //  } else {
    //
    //    // use '\uFFFF' as end-of-string marker
    //    c = END_OF_STRING;
    //  }
    //}
    //throw new ShouldNeverHappenException("hostnameOrUsername");
    //
    //// parse hostname -OR- user-info AFTER we're parsed a colon (':')
    //hostnameOrUserInfoAfterColon:
    //for(;;) {
    //  ++current;
    //  if(current < length) {
    //    c = text[current];
    //    switch(c) {
    //      case '%':
    //      case '+':
    //      decode = true;
    //      break;
    //    }
    //  } else {
    //
    //    // use '\uFFFF' as end-of-string marker
    //    c = END_OF_STRING;
    //  }
    //  if(
    //    ((c >= 'a') && (c <= 'z')) ||
    //      ((c >= 'A') && (c <= 'Z')) ||
    //      ((c >= '0') && (c <= '9')) ||
    //      ((c >= '$') && (c <= '.')) ||   // one of: $%&'()*+,-.
    //      (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') ||
    //      char.IsLetterOrDigit(c)
    //  ) {
    //
    //    // valid character, keep parsing
    //  } else if(c == '@') {
    //
    //    // part before ':' was username
    //    user = hostnameOrUsername;
    //    if(decode) {
    //      user = Decode(user);
    //    }
    //
    //    // part after ':' is password
    //    password = text.Substring(last, current - last);
    //    if(decode) {
    //      password = Decode(password);
    //    }
    //    last = current + 1;
    //    decode = false;
    //    goto hostnameOrIPv6Address;
    //  } else if((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //
    //    // part before ':' was hostname
    //    if(decode) {
    //
    //      // hostname cannot contain encoded characters
    //      return -1;
    //    }
    //    hostname = hostnameOrUsername;
    //
    //    // part after ':' is port, parse and validate it
    //    if(!int.TryParse(text.Substring(last, current - last), out port) || (port < 0) || (port > ushort.MaxValue)) {
    //      return -1;
    //    }
    //    next = (nextStep)c;
    //    return current + 1;
    //  } else {
    //    return -1;
    //  }
    //}
    //throw new ShouldNeverHappenException("hostnameOrUserInfoAfterColon");
    //
    //hostnameOrIPv6Address:
    //  ++current;
    //if(current < length) {
    //  c = text[current];
    //  switch(c) {
    //    case '%':
    //    case '+':
    //    decode = true;
    //    break;
    //    case '[':
    //
    //      // NOTE (steveb): we want to include the leading character in the final result
    //      last = current;
    //
    //    // IPv6 addresses start with '['
    //    goto ipv6;
    //  }
    //} else {
    //
    //  // use '\uFFFF' as end-of-string marker
    //  c = END_OF_STRING;
    //}
    //for(;;) {
    //  if(
    //    ((c >= 'a') && (c <= 'z')) ||
    //      ((c >= 'A') && (c <= 'Z')) ||
    //      ((c >= '0') && (c <= '9')) ||
    //      ((c >= '$') && (c <= '.')) ||   // one of: $%&'()*+,-.
    //      (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') ||
    //      char.IsLetterOrDigit(c)
    //  ) {
    //
    //    // valid character, keep parsing
    //  } else if(c == ':') {
    //    if(decode) {
    //
    //      // hostname cannot contain encoded characters
    //      return -1;
    //    }
    //    hostname = text.Substring(last, current - last);
    //    last = current + 1;
    //    goto portNumber;
    //  } else if((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //    if(decode) {
    //
    //      // hostname cannot contain encoded characters
    //      return -1;
    //    }
    //    hostname = text.Substring(last, current - last);
    //    next = (nextStep)c;
    //    return current + 1;
    //  } else {
    //    return -1;
    //  }
    //
    //  // continue on by reading the next character
    //  ++current;
    //  if(current < length) {
    //    c = text[current];
    //    switch(c) {
    //      case '%':
    //      case '+':
    //      decode = true;
    //      break;
    //    }
    //  } else {
    //
    //    // use '\uFFFF' as end-of-string marker
    //    c = END_OF_STRING;
    //  }
    //}
    //throw new ShouldNeverHappenException("hostname");
    //
    //portNumber:
    //for(;;) {
    //  ++current;
    //  c = (current < length) ? text[current] : END_OF_STRING;
    //  if((c >= '0') && (c <= '9')) {
    //
    //    // valid character, keep parsing
    //  } else if((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //    if(!int.TryParse(text.Substring(last, current - last), out port) || (port < 0) || (port > ushort.MaxValue)) {
    //      return -1;
    //    }
    //    next = (nextStep)c;
    //    return current + 1;
    //  } else {
    //    return -1;
    //  }
    //}
    //throw new ShouldNeverHappenException("portNumber");

  }

  def TryParseIPv6(text: String, length: Int, current1: Int, parts: Uri): Option[StepResult] = {
    //      ipv6:
    //      for(;;) {
    //        ++current;
    //        c = (current < length) ? text[current] : END_OF_STRING;
    //        if(((c >= 'a') && (c <= 'f')) || ((c >= 'A') && (c <= 'F')) || ((c >= '0') && (c <= '9')) || (c == ':') || (c == '.')) {
    //
    //          // valid character, keep parsing
    //        } else if(c == ']') {
    //          hostname = text.Substring(last, current - last + 1);
    //
    //          // check next character to determine correct state to transition to
    //          ++current;
    //          c = (current < length) ? text[current] : END_OF_STRING;
    //          if(c == ':') {
    //            last = current + 1;
    //            goto portNumber;
    //          } else if((c == '/') || (c == '\\') || (c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //            next = (nextStep)c;
    //            return current + 1;
    //          } else {
    //            return -1;
    //          }
    //        } else {
    //          return -1;
    //        }
    //      }
    //      throw new ShouldNeverHappenException("ipv6");
    None
  }

  def TryParsePath(text: String, length: Int, current: Int, parts: Uri): StepResult = {
    //      next = nextStep.Error;
    //      segments = null;
    //      var last = current;
    //      var hasLeadingBackslashes = false;
    //      var segmentList = new List<string>(16);
    //      var leading = true;
    //      char c;
    //      for(; ; ++current) {
    //        c = (current < length) ? text[current] : END_OF_STRING;
    //        if((c == '/') || (c == '\\')) {
    //          if(leading) {
    //            hasLeadingBackslashes = hasLeadingBackslashes || (c == '\\');
    //          } else {
    //            var segment = text.Substring(last, current - last);
    //            if(hasLeadingBackslashes) {
    //              segment = segment.Replace('\\', '/');
    //              hasLeadingBackslashes = false;
    //            }
    //            segmentList.Add(segment);
    //            last = current + 1;
    //            leading = true;
    //          }
    //        } else if(
    //          ((c >= 'a') && (c <= '~')) ||   // one of: abcdefghijklmnopqrstuvwxyz{|}~
    //            ((c >= '@') && (c <= '_')) ||   // one of: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
    //            ((c >= '$') && (c <= ';')) ||   // one of: $%&'()*+,-./0123456789:;
    //            (c == '=') || (c == '!') ||
    //            char.IsLetterOrDigit(c)
    //        ) {
    //
    //          // no longer accept leading '/' or '\' characters
    //          leading = false;
    //        } else if((c == '?') || (c == '#') || (c == END_OF_STRING)) {
    //          if(last == current) {
    //            trailingSlash = true;
    //          } else {
    //            var segment = text.Substring(last, current - last);
    //            if(hasLeadingBackslashes) {
    //              segment = segment.Replace('\\', '/');
    //            }
    //            segmentList.Add(segment);
    //          }
    //
    //          // we're done parsing the path string
    //          break;
    //        } else {
    //          return -1;
    //        }
    //      }
    //
    //      // initialize return values
    //      segments = segmentList.ToArray();
    //      next = (nextStep)c;
    //      return current + 1;
    StepResult(0, Steps.End, Uri("x"))
  }

  def TryParseQuery(text: String, length: Int, current: Int, parts: Uri): StepResult = {
    //      next = nextStep.Error;
    //      @params = null;
    //      var last = current;
    //      var paramsList = new List<KeyValuePair<string, string>>(16);
    //      string paramsKey = null;
    //      var decode = false;
    //      var parsingKey = true;
    //      char c;
    //      for(; ; ++current) {
    //        if(current < length) {
    //          c = text[current];
    //          switch(c) {
    //            case '%':
    //            case '+':
    //            decode = true;
    //            break;
    //          }
    //        } else {
    //
    //          // use '\uFFFF' as end-of-string marker
    //          c = END_OF_STRING;
    //        }
    //        if(
    //          ((c >= 'a') && (c <= '~')) || // one of: abcdefghijklmnopqrstuvwxyz{|}~
    //            ((c >= '?') && (c <= '_')) || // one of: ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
    //            ((c >= '\'') && (c <= ';')) || // one of: '()*+,-./0123456789:;
    //            (c == '$') || (c == '%') || (c == '!') ||
    //            char.IsLetterOrDigit(c)
    //        ) {
    //
    //          // valid character, keep parsing
    //        } else if((c == '&') || (c == '#') || (c == END_OF_STRING)) {
    //          if(parsingKey) {
    //            if(current != last) {
    //
    //              // add non-empty key with empty value
    //              paramsKey = text.Substring(last, current - last);
    //              if(decode) {
    //                paramsKey = Decode(paramsKey);
    //                decode = false;
    //              }
    //              paramsList.Add(new KeyValuePair<string, string>(paramsKey, null));
    //            } else if(c == '&') {
    //
    //              // this occurs in the degenerate case of two consecutive ampersands (e.g. "&&")
    //              paramsList.Add(new KeyValuePair<string, string>("", null));
    //            }
    //          } else {
    //
    //            // add key with value
    //            var paramsValue = text.Substring(last, current - last);
    //            if(decode) {
    //              paramsValue = Decode(paramsValue);
    //              decode = false;
    //            }
    //            paramsList.Add(new KeyValuePair<string, string>(paramsKey, paramsValue));
    //            parsingKey = true;
    //          }
    //
    //          // check if we found a query parameter separator
    //          if(c == '&') {
    //            last = current + 1;
    //            continue;
    //          }
    //
    //          // we're done parsing the query string
    //          break;
    //        } else if(c == '=') {
    //          if(parsingKey) {
    //            paramsKey = text.Substring(last, current - last);
    //            if(decode) {
    //              paramsKey = Decode(paramsKey);
    //              decode = false;
    //            }
    //            last = current + 1;
    //            parsingKey = false;
    //          }
    //        } else {
    //          return -1;
    //        }
    //      }
    //
    //      // initialize return values
    //      next = (nextStep)c;
    //      @params = paramsList.ToArray();
    //      return current + 1;
    StepResult(0, Steps.End, Uri("x"))
  }

  def TryParseFragment(text: String, length: Int, start: Int, parts: Uri): StepResult = {
    def ParseFragment(current: Int, decode: Boolean): StepResult = {
    //var decode = false;
      if (current < length) {
        val c = text(current)
        val decode2 = c match {
          case '%' => true
          case '+' => true
          case _ => decode
        }
        if(IsFragmentChar(c)) {
          ParseFragment(current+1,decode2)
        } else {
          StepResult(current,Steps.Error,parts)
        }
      } else {
        val fragment = text.substring(start, current)
        val fragment2 = if (decode) {
          Decode(fragment)
        } else fragment
        StepResult(current, Steps.End,parts.copy(fragment = Some(fragment2)))
      }
    }
    ParseFragment(start+1, false)
    StepResult(0, Steps.End, Uri("x"))
  }

  def IsFragmentChar(c: Char): Boolean = {

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

  def Decode(text: String): String = {
    val length = text.length
    def Decode2(textIndex: Int, bytes: List[Byte]): List[Byte] = {
      if (textIndex >= length) {
        bytes
      } else {
        val c = text(textIndex)
        c match {
            // + => space
          case '+' => Decode2(textIndex + 1, ' '.toByte :: bytes)
            // % followed
          case '%' =>
            // if(((textIndex + 2) < length) && ((next = text[textIndex + 1]) != '%')) {
            if (textIndex + 2 < length) {
              text(textIndex + 1) match {
                  // fall through to default char handler
                case '%' => Decode2(textIndex + 1, c.toByte:: bytes)
                // if(next == 'u') {
                case 'u' =>
                  // if(((textIndex + 5) < length) && (xchar = GetChar(text, textIndex + 2, 4)) != -1) {
                  if (textIndex + 5 < length) {
                    val xchar = GetChar(text, textIndex + 2, 4)
                    if( xchar == -1) {
                      // fall through to default char handler
                      Decode2(textIndex + 1, c.toByte:: bytes)
                    } else {
                      //chars[0] = (char)xchar;
                      //bytesIndex += Encoding.UTF8.GetBytes(chars, 0, 1, bytes, bytesIndex);
                      //textIndex += 5;
                      //continue;
                      val charBytes = xchar.toChar.toString.getBytes(StandardCharsets.UTF_8)
                      Decode2(textIndex + 2 + charBytes.length, charBytes.reverse.toList ::: bytes)
                    }
                  } else {
                    // fall through to default char handler
                    Decode2(textIndex + 1, c.toByte:: bytes)
                  }
                case _ =>
                  //bytes[bytesIndex++] = (byte)xchar;
                  //textIndex += 2;
                val xchar = GetChar(text, textIndex + 1, 2)
                  val charBytes = xchar.toChar.toString.getBytes(StandardCharsets.UTF_8)
                  Decode2(textIndex + 1 + charBytes.length, charBytes.reverse.toList ::: bytes)
              }
            } else {
              Decode2(textIndex + 1, c.toByte:: bytes)
            }
          case _ => Decode2(textIndex + 1, c.toByte :: bytes)
        }
      }
    }
    val bytes = Decode2(0,Nil).reverse.toArray
    new String(bytes, StandardCharsets.UTF_8)
  }

  def DeterminePort(uriParts: Uri): Uri = uriParts.port match {
    case None =>
        val port = uriParts.scheme.toLowerCase.hashCode match {
          case LOCAL_HASHCODE => None
          case HTTP_HASHCODE => Some(80)
          case HTTPS_HASHCODE => Some(443)
          case FTP_HASHCODE => Some(21)
        }
        uriParts.copy(port = port, usesDefaultPort = true)
    case Some(port) if uriParts.usesDefaultPort => uriParts.copy(usesDefaultPort = false)
    case _ => uriParts
  }

  def GetChar(text: String, start: Int, length: Int): Int = {
    def GetChar2(offset: Int, result: Int): Int = {
      if (start + length <= offset) {
        result
      } else {
        val value = text(offset) match {
          case c if c >= '0' && c <= '9' => c - '0'
          case c if c >= 'a' && c <= 'f' => c - 'a' + 10
          case c if c >= 'A' && c <= 'F' => c - 'A' + 10
          case _ => -1
        }
        if (value == -1) value else GetChar2(offset + 1, (result << 4) + value)
      }
    }
    GetChar2(start, 0)
  }
}
