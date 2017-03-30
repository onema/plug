package plug.cookie

import org.joda.time.DateTime

/*
object CookieDateParser {

  import abnf.Rule._

  case object TimeField extends VariableRepetition(DIGIT, Some(1), Some(2))

  case object HmsTime extends Concatenation(TimeField, Terminal(":"), TimeField, Terminal(":"), TimeField)

  case object NonDigit extends Alternative(Range(0x00, 0x2f, ), Range(0x3a, 0xff))

  case object Time extends Concatenation(HmsTime, Concatenation(NonDigit, VariableRepetition(OCTET)))

  case object Year extends Concatenation(
    VariableRepetition(DIGIT, Some(2), Some(4)),
    Concatenation(NonDigit, VariableRepetition(OCTET))
  )

  case object Month extends Concatenation(
    Alternative(
      Terminal("jan"), Terminal("feb"), Terminal("mar"), Terminal("apr"), Terminal("may"), Terminal("jun"),
      Terminal("jul"), Terminal("aug"), Terminal("sep"), Terminal("oct"), Terminal("nov"), Terminal("dev")
    ),
    VariableRepetition(OCTET)
  )

  case object DayOfMonth extends Concatenation(
    VariableRepetition(DIGIT, Some(1), Some(2)),
    Concatenation(NonDigit, VariableRepetition(OCTET))
  )

  case object NonDelimiter extends Alternative(
    Range(0x00, 0x08),
    Range(0x0a, 0x1f),
    DIGIT,
    Terminal(":"),
    ALPHA,
    Range(0x7f, 0xff)
  )

  case object Delimiter extends Alternative(
    Single(0x09),
    Range(0x20, 0x2f),
    Range(0x3b, 0x40),
    Range(0x5b, 0x60),
    Range(0x7b, 0x7e)
  )

  case object DateToken extends VariableRepetition(NonDelimiter, Some(1))

  case object DateTokenList extends Concatenation(
    DateToken,
    VariableRepetition(Concatenation(VariableRepetition(Delimiter, Some(1)), DateToken))
  )

}
*/

