package plug

/**
  * Created by arne on 1/3/17.
  */
object StringExtensions {

  implicit class StringEscape(text: String) {

    def escapeString = if (text == null || text.isEmpty) ""
    else {
      // escape any special characters
      val result = new StringBuilder(2 * text.length)
      text.foreach {
        case '\u0001' => result.append("\\a")
        case '\b' => result.append("\\b")
        case '\f' => result.append("\\f")
        case '\n' => result.append("\\n")
        case '\r' => result.append("\\r")
        case '\t' => result.append("\\t")
        case '\u0016' => result.append("\\v")
        case '"' => result.append("\\\"")
        case '\'' => result.append("\\'")
        case '\\' => result.append("\\\\")
        case c if c < 32 || c >= 127 =>
          result.append("\\u")
          result.append("00")
          result.append(Integer.toHexString(c))
        case c => result.append(c)
      }
      result.toString()
    }

    def unescapeString: String = {
      val result = new StringBuilder(text.length)
      def unescape(i: Int): Unit = if (i < text.length) {
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
