package plug


import java.util.Locale

import org.joda.time.{DateTime, DateTimeZone}
import StringExtensions.StringEscape
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.util.{Failure, Success, Try}

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

  /**
    * Parse a collection cookies from a set cookie header.
    *
    * @param setCookieHeader Http set cookie header.
    * @return Collection of cookies.
    */
  def parseSetCookieHeader(setCookieHeader: String): List[Cookie] = setCookieHeader match {
    case null => Nil
    case x if x.isEmpty => Nil
    case _ => Internals.parseSetCookie(setCookieHeader, 0, Nil)
  }

  object Internals {
    def parseCookie(text: String, index: Int, acc: List[Cookie]): List[Cookie] = if (index + 1 >= text.length) {
      acc.reverse
    } else parseNameValue(text, index, useCommaAsSeparator = true) match {
      case (None, index1) => parseCookie(text, index1, acc)
      case (Some((cookieName, cookieValue)), index1) =>

        def buildUri(domainOption: Option[String], pathOption: Option[String]): Option[Uri] = (domainOption, pathOption) match {
          case (None, None) => None
          case (domain, path) => Uri.fromString(s"http://${domain.getOrElse("")}${path.getOrElse("")}")
        }

        def parse(index: Int, domain: Option[String], path: Option[String]): (Option[Uri], Int) =
          parseNameValue(text, index, useCommaAsSeparator = true) match {
            case (None, idx1) => (buildUri(domain, path), idx1)
            case (Some(nameValuePair), idx1) => nameValuePair match {
              case ("$Path", v) => parse(idx1, domain, Some(v))
              case ("$Domain", v) => parse(idx1, Some(v), path)
              case _ => (buildUri(domain, path), index)
            }
          }

        val (uri, index2) = parse(index1, None, None)
        parseCookie(text, skipComma(text, index2), Cookie(cookieName, cookieValue, uri) :: acc)
    }

    case class SetCookieParts(domain: Option[String] = None,
                              path: Option[String] = None,
                              expires: Option[DateTime] = None,
                              version: Option[Int] = None,
                              comment: Option[String] = None,
                              commentUri: Option[Uri] = None,
                              discard: Boolean = false,
                              secure: Boolean = false,
                              httpOnly: Boolean = false) {
      def uri: Option[Uri] = (domain, path) match {
        case (None, None) => None
        case (d, p) =>
          val d2 = d.map { x => if (x(0) == '.') x.substring(1) else x } // stripping . from .example.net
          Uri.fromString(s"http://${d2.getOrElse("")}${p.getOrElse("")}")
      }
    }

    def parseSetCookie(text: String, index: Int, acc: List[Cookie]): List[Cookie] = if (index + 1 >= text.length) {
      acc.reverse
    } else parseNameValue(text, index, useCommaAsSeparator = true) match {
      case (None, index1) => acc.reverse // unlike parseCookie parseSetCookie drops out on first missed match
      case (Some((cookieName, cookieValue)), index1) =>

        def parse(index: Int, parts: SetCookieParts): (SetCookieParts, Int) =
          parseNameValue(text, index, useCommaAsSeparator = true) match {
            case (None, idx1) => (parts, idx1)
            case (Some(nameValuePair), idx1) => nameValuePair.copy(_1 = nameValuePair._1.toLowerCase) match {
              case ("path", v) => parse(idx1, parts.copy(path = Some(v)))
              case ("domain", v) => parse(idx1, parts.copy(domain = Some(v)))
              case ("comment", v) => parse(idx1, parts.copy(comment = Some(v)))
              case ("commenturl", v) => parse(idx1, parts.copy(commentUri = Uri.fromString(v)))
              case ("max-age", v) => Try(Integer.parseInt(v)) match {
                case Success(i) =>
                  val expires = new DateTime(DateTimeZone.UTC).plusSeconds(i)
                  parse(idx1, parts.copy(expires = Some(expires)))
                case _ => parse(idx1, parts) // ignoring integer parse failure
              }
              case ("expires", v) => parse(idx1, parts.copy(expires = parseCookieDateTimeString(v)))
              case ("version", v) => Try(Integer.parseInt(v)) match {
                case Success(i) => parse(idx1, parts.copy(version = Some(i)))
                case _ => parse(idx1, parts) // ignoring integer parse failure
              }
              case ("discard", v) => parse(idx1, parts.copy(discard = true))
              case ("secure", v) => parse(idx1, parts.copy(secure = true))
              case ("httponly", v) => parse(idx1, parts.copy(httpOnly = true))
              case _ => parse(idx1, parts) // unrecognized, skipping
            }
          }

        val (parts, index2) = parse(index1, SetCookieParts())
        val cookie = Cookie(
          cookieName,
          cookieValue,
          uri = parts.uri,
          expires = parts.expires,
          secure = parts.secure,
          comment = parts.comment,
          commentUri = parts.commentUri,
          version = parts.version,
          discard = parts.discard,
          httpOnly = parts.httpOnly,
          setCookie = true
        )
        parseSetCookie(text, skipComma(text, index2), cookie :: acc)
    }

    def parseCookieDateTimeString(cookieExpires: String): Option[DateTime] = {
      val formatter = DateTimeFormat.forPattern("EEE, dd-MMM-yyyy HH:mm:ss 'GMT'").withZoneUTC()
      Try(DateTime.parse(cookieExpires, formatter)) match {
        case Success(dt) => Some(dt)
        case Failure(e) =>
          println(e)
          None
      }
    }

    def parseNameValue(text: String, index: Int, useCommaAsSeparator: Boolean): (Option[(String, String)], Int) = {

      def findEquals(index: Int): (Boolean, Int) =
        if (index >= text.length) (false, index)
        else if (text(index) == '=') (true, index + 1)
        else (false, index)

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
            parseValue(text, index3, useCommaAsSeparator) match {
              case (None, index4) => (None, index4)
              case (Some((value, false)), index4) if useCommaAsSeparator && index4 < text.length && text(index4) == ',' =>

                def indexOfEqualOrSemiColon(): Int = {
                  (text.indexOf('=', index4), text.indexOf(';', index4)) match {
                    case (-1, -1) => -1
                    case (-1, b) => b
                    case (a, -1) => a
                    case (a, b) => math.min(a, b)
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
                consumeTail(value1, index5)
              case (Some((value, _)), index4) => consumeTail(value, index4)
            }
          } else {
            consumeTail(null, index3) // TODO: null value? do we need to make .value an Option?
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
          (Some(text.substring(wordStart).unescapeString), last)
        } else {
          (Some(text.substring(wordStart, last).unescapeString), last + 1)
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
          (Some((text.substring(wordStart).unescapeString, true)), last)
        } else {
          (Some((text.substring(wordStart, last).unescapeString, true)), last + 1)
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

  }

}
