package plug

import org.joda.time.DateTime

/**
  * Created by arne on 12/31/16.
  */
object CookieParser {

  /**
    * Create a cookie collection from a cookie header.
    *
    * @param header Http cookie header.
    * @return Cookie collection.
    */
  def parseCookieHeader(header: String): List[Cookie] = header match {
    case null => Nil
    case x if x.isEmpty => Nil
    case _ => Internals.parseNameValue(header, 0, useCommaAsSeparator = false) match {
      case (None, _) => Nil
      case (Some((name, value)), index) =>
        // check if we read the cookie version information
        val nextIndex = if (name.compareToIgnoreCase("$Version") != 0) 0 else index
        Internals.parseCookie(header, nextIndex, Nil)
    }
  }

  //    List<DreamCookie> result = new List<DreamCookie>();
  //    if(string.IsNullOrEmpty(header)) {
  //      return result;
  //    }
  //    int index = 0;
  //    string name;
  //    string value;
  //    if(!ParseNameValue(out name, out value, header, ref index, false)) {
  //      return result;
  //    }
  //
  //    // check if we read the cookie version information
  //    if(string.Compare(name, "$Version", true) != 0) {
  //
  //      // we read something else; let's forget that we read it
  //      index = 0;
  //    }
  //    while(index < header.Length - 1) {
  //      DreamCookie cookie = ParseCookie(header, ref index);
  //      if(cookie != null) {
  //        result.Add(cookie);
  //      }
  //    }
  //    return result;
  //  }

  /**
    * Parse a collection cookies from a set cookie header.
    *
    * @param setCookieHeader Http set cookie header.
    * @return Collection of cookies.
    */
  def parseSetCookieHeader(setCookieHeader: String): List[Cookie] = ???

  //    List<DreamCookie> result = new List<DreamCookie>();
  //    if(string.IsNullOrEmpty(setCookieHeader)) {
  //      return result;
  //    }
  //    int index = 0;
  //    DreamCookie cookie;
  //    while((cookie = ParseSetCookie(setCookieHeader, ref index)) != null) {
  //      result.Add(cookie);
  //    }
  //    return result;
  //  }

  object Internals {
    def parseCookie(text: String, index: Int, acc: List[Cookie]): List[Cookie] = if (index + 1 >= text.length) {
      acc
    } else parseNameValue(text, index, useCommaAsSeparator = true) match {
      case (None, index1) => parseCookie(text, index1, acc)
      case (Some((cookieName, cookieValue)), index1) =>

        def buildUri(domainOption: Option[String], pathOption: Option[String]): Option[Uri] = (domainOption, pathOption) match {
          case (None, None) => None
          case (domain, path) => Uri.fromString(s"http://${domain.getOrElse("")}${path.getOrElse("")}")
        }

        def parse(index: Int, domain: Option[String], path: Option[String]): (Option[Uri], Int) =
          parseNameValue(text, index, useCommaAsSeparator = true) match {
            case (None, idx1) => (buildUri(path, domain), idx1)
            case (Some(nameValuePair), idx1) => nameValuePair match {
              case ("$Path", v) => parse(idx1, domain, Some(v))
              case ("$Domain", v) => parse(idx1, Some(v), path)
            }
          }

        val (uri, index2) = parse(index1, None, None)
        parseCookie(text, skipComma(text, index2), Cookie(cookieName, cookieValue, uri) :: acc)
    }

    def parseSetCookie(text: String, index: Int): Option[(Cookie, Int)] = ???

    //    string cookieName;
    //    string cookieValue;
    //    DateTime expires = DateTime.MaxValue;
    //    int version = 1;
    //    string domain = string.Empty;
    //    string path = string.Empty;
    //    string comment = string.Empty;
    //    XUri commentUri = null;
    //    bool discard = false;
    //    bool secure = false;
    //    bool httpOnly = false;
    //    if(!ParseNameValue(out cookieName, out cookieValue, text, ref index, true)) {
    //      return null;
    //    }
    //
    //    string name;
    //    string value;
    //    while(ParseNameValue(out name, out value, text, ref index, true)) {
    //      if(StringUtil.EqualsInvariantIgnoreCase(name, "comment")) {
    //        comment = value;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "commenturl")) {
    //        commentUri = new XUri(value);
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "domain")) {
    //        domain = value;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "path")) {
    //        path = value;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "max-age")) {
    //        expires = GlobalClock.UtcNow.AddSeconds(int.Parse(value, NumberFormatInfo.InvariantInfo));
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "expires")) {
    //        expires = ParseCookieDateTimeString(value);
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "port")) {
    //
    //        // TODO (steveb): why is this commented out?
    //        // result.Port = value;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "version")) {
    //        version = int.Parse(value, NumberFormatInfo.InvariantInfo);
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "discard")) {
    //        discard = true;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "secure")) {
    //        secure = true;
    //      } else if(StringUtil.EqualsInvariantIgnoreCase(name, "httponly")) {
    //        httpOnly = true;
    //      } else {
    //
    //        // unrecognized attribute; let's skip it
    //      }
    //    }
    //    SkipComma(text, ref index);
    //
    //    // TODO (steveb): why are we doing this?
    //    XUri uri = null;
    //    if((domain != null) || (path != null)) {
    //      if((domain != null) && domain.StartsWith(".")) {
    //
    //        // TODO (steveb): why are we modifying the original domain value?
    //        domain = domain.Remove(0, 1);
    //      }
    //
    //      // TODO (steveb): the produced URI always uses 'http' even when the cookie is secure, why?
    //      uri = new XUri(string.Format("http://{0}{1}", domain, path));
    //    }
    //    return new DreamCookie(cookieName, cookieValue, uri, expires, version, secure, discard, comment, commentUri, httpOnly, false);
    //  }

    def parseCookieDateTimeString(cookieExpires: String): Option[DateTime] = ???

    //    DateTime ret;
    //    if(!DateTimeUtil.TryParseExactInvariant(cookieExpires, "ddd, dd-MMM-yyyy HH:mm:ss 'GMT'", out ret)) {
    //      ret = DateTime.MaxValue;
    //    }
    //    return ret;
    //  }

    def parseNameValue(text: String, index: Int, useCommaAsSeparator: Boolean): (Option[(String, String)], Int) = {
      def findEquals(index: Int): (Boolean, Int) = if (text(index) == '=') (true, index + 1) else (false, index)
      parseWord(text, skipWhitespace(text, index)) match {
        case (None, index2) => (None, index2)
        case (Some(name), index2) =>
          val (matchedEquals, index3) = findEquals(skipWhitespace(text, index2))

          def consumeTail(value: String, index: Int): (Option[(String, String)], Int) = {
            val index1 = if (useCommaAsSeparator) {
              skipSemiColon(text, skipWhitespace(text, index))
            } else {
              skipComma(text, skipWhitespace(text, index))
            }
            (Some((name, value)), index1)
          }

          if (matchedEquals) {
            parseValue(text, index, useCommaAsSeparator) match {
              case (None, index4) => (None, index4)
              case (Some((value, false)), index4) if useCommaAsSeparator && index4 < text.length && text(index4) == ',' =>

                def indexOfEqualOrSemiColon(): Int = {
                  (text.indexOf('=', index4), text.indexOf(';', index4)) match {
                    case (-1, -1) => -1
                    case (a, b) if a > b => a
                    case (_, b) => b
                  }
                }

                val next = indexOfEqualOrSemiColon()
                val (value1, index5) = if (next < 0) {
                  (value + text.substring(index4), text.length)
                } else if (next > 0 && text(next) == ';') {
                  (value + text.substring(index4, next), next)
                } else {
                  (value, index4)
                }
                (Some((name, value1)), index5)
              case (Some((value, _)), index4) => (Some((name, value)), index4)
            }
          } else {
            consumeTail(null, index3) // null value? what does that mean?
          }
      }
    }

    def skipDelimiter(text: String, index: Int): Int =
      if ((index < text.length) && ((text(index) == ',') || (text(index) == ';'))) index + 1 else index

    def skipWhitespace(text: String, index: Int): Int = {
      var index1 = index // TODO: factor out the var
      while (index1 < text.length && Character.isWhitespace(text(index1))) {
        index1 += 1
      }
      index1
    }

    def skipSemiColon(text: String, index: Int): Int =
      if ((index < text.length) && (text(index) == ';')) index + 1 else index

    def skipComma(text: String, index: Int): Int =
      if ((index < text.length) && (text(index) == ',')) index + 1 else index

    def parseWord(text: String, index: Int): (Option[String], Int) =
      if (index >= text.length) {
        (None, index)
      } else if (text(index) == '"') {
        // parsing a quoted string

        def scanQuotedWord(index: Int): Int =
          if (index >= text.length) {
            index
          } else if (text(index) == '"') {
            index
          } else if (text(index) == '\\') {
            if (index + 1 == text.length) {
              index + 1
            } else {
              scanQuotedWord(index + 2)
            }
          } else {
            scanQuotedWord(index + 1)
          }

        val wordStart = index + 1
        val last = scanQuotedWord(wordStart)
        if (last == text.length) {
          (Some(unescapeString(text.substring(wordStart))), last)
        } else {
          (Some(unescapeString(text.substring(wordStart, last))), last + 1)
        }
      } else {
        // parse an alphanumeric token

        def scanWord(index: Int): Int =
          if (index >= text.length) {
            index
          } else if (!isTokenChar(text(index))) {
            index
          } else {
            scanWord(index + 1)
          }

        val last = scanWord(index)
        if (last == index) {
          // If we have an invalid character here, we need to move on
          (None, index + 1)
        } else {
          (Some(text.substring(index, last)), last)
        }
      }

    def parseValue(text: String, index: Int, useCommaAsSeparator: Boolean): (Option[(String, Boolean)], Int) =
      if (index >= text.length) {
        (None, index)
      } else if (text(index) == '"') {

        // we're parsing a quoted string
        def scanQuotedWord(index: Int): Int =
          if (index >= text.length) {
            index
          } else if (text(index) == '"') {
            index
          } else if (text(index) == '\\') {
            if (index + 1 == text.length) {
              index + 1
            } else {
              scanQuotedWord(index + 2)
            }
          } else {
            scanQuotedWord(index + 1)
          }

        val wordStart = index + 1
        val last = scanQuotedWord(wordStart)
        if (last == text.length) {
          (Some((unescapeString(text.substring(wordStart)), true)), last)
        } else {
          (Some((unescapeString(text.substring(wordStart, last)), true)), last + 1)
        }
      } else {
        // parse an alphanumeric token

        def scanWord(index: Int): Int =
          if (index >= text.length) {
            index
          } else if (text(index) == ';') {
            index
          } else if (useCommaAsSeparator && text(index) == ',') {
            index
          } else {
            scanWord(index + 1)
          }

        val last = scanWord(index)
        (Some((text.substring(index, last).trim, false)), last)
      }

    def isTokenChar(c: Char) = ((c >= 'A') && (c <= 'Z')) ||
      ((c >= 'a') && (c <= 'z')) ||
      ((c > 32) && (c < 127) && (c != ',') && (c != ';') && (c != '='))

    def unescapeString(text: String): String = {
      val result = new StringBuilder(text.length)
      def unescape(i: Int): Unit = {
        val c = text(i)
        if (c == '\\' && i + 1 < text.length) {
          text(i + 1) match {
            case 'a' =>
              result.append('\u0001') // \a
              unescape(i + 2)
            case 'b' =>
              result.append('\b')
              unescape(i + 2)
            case 'f' =>
              result.append('\f')
              unescape(i + 2)
            case 'n' =>
              result.append('\n')
              unescape(i + 2)
            case 'r' =>
              result.append('\r')
              unescape(i + 2)
            case 't' =>
              result.append('\t')
              unescape(i + 2)
            case 'u' =>
              result.append(Integer.parseInt(text.substring(i + 1, 4), 16))
              unescape(i + 6)
            case 'v' =>
              result.append('\u0016') // \v
              unescape(i + 2)
            case x =>
              result.append(x)
              unescape(i + 2)
          }
        } else {
          result.append(c)
          unescape(i + 1)
        }
      }
      unescape(0)
      result.toString()
    }
  }

}
