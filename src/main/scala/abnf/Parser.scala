package abnf

trait Token

object Token {

  case class Single(c: Char) extends Token

  case class ALPHA(c: Char) extends Token

  case class DIGIT(int: Int) extends Token

  case class OCTET(c: Char) extends Token

  case object SP extends Token

  case object CR extends Token

  case object LF extends Token

  case object CRLF extends Token

  case class VCHAR(c: Char) extends Token

  case class Terminal(value: String) extends Token

  case class Repetition(tokens: List[Token]) extends Token

  case class Concatenation(tokens: List[Token]) extends Token

  case class OptionalSequence(tokens: List[Token]) extends Token

  object BIT {
    def apply(c: Char) = new BIT(c == '1')
  }
  case class BIT(set: Boolean) extends Token

  case class CHAR(c: Char) extends Token

  case class CTL(c: Char) extends Token

  case object DQUOTE extends Token

  case object HTAB extends Token

}

trait Rule {
  def getParser: RuleParser
}

//class StringParser[A](grammar: Rule) {
//  def parse(input: String): StringParserResult
//}

//sealed trait StringParserResult
//
//case class Success[A](token: A) extends StringParserResult
//
//case class Failure(position: Int, rule: Rule) extends StringParserResult

trait RuleParser {
  def parse(input: Char): ParseResult

  def rule: Rule

}

object RuleParser

sealed trait ParseResult

case class Partial(parser: RuleParser) extends ParseResult

case class Match(token: Token, parser: Option[RuleParser] = None) extends ParseResult

case object Error extends ParseResult

//class CustomRule(wrapped: Rule) extends Rule {
//  def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] =
//    new PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] {
//      override def isDefinedAt(x: (Int, Either[Token, Rule])): Boolean = false
//
//      override def apply(v1: (Int, Either[Token, Rule])): (Int, Either[Token, Rule]) = v1
//    }
//
//  val default: PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
//    case x => x
//  }
//
//  override def parse(input: String, position: Int): (Int, Either[Token, Rule]) =
//    (matcher(input, position) orElse default) (wrapped.parse(input, position))
//
//}
//
//class CustomRule2(wrapped: Rule)
//                 (matcher: (String, Int) => PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])])
//  extends Rule {
//
//  val default: PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
//    case x => x
//  }
//
//  override def parse(input: String, position: Int): (Int, Either[Token, Rule]) =
//    (matcher(input, position) orElse default) (wrapped.parse(input, position))
//
//}
//
//abstract class CustomRule3(wrapped: Rule) extends Rule {
//  def intercept(input: String, start: Int, end: Int, token: Token): Either[Token, Rule]
//
//  override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = wrapped.parse(input, position) match {
//    case (end, Left(token)) => (end, intercept(input, position, end, token))
//    case x => x
//  }
//
//}

object Rule {

  object Alternative {
    def apply(rules: Rule*): Alternative = new Alternative(rules: _*)
  }

  implicit val defaultToken: Option[(Token => Token)] = None

  class Alternative(rules: Rule*) extends Rule {
    override def getParser: RuleParser = ???
  }

  object Concatenation {
    def apply(rules: Rule*): Concatenation = new Concatenation(rules: _*)
  }

  class Concatenation(rules: Rule*) extends Rule {

    class ConcatentionParser(parser: RuleParser, rules: Seq[Rule], tokens: List[Token], previousMatch: Option[Token]) extends RuleParser {

      override def parse(input: Char): ParseResult = parser.parse(input) match {
        case x:Error => previousMatch match {
          case None => x
          case Some(token) => rules match {
            case Nil => Match(Token.Concatenation((token::tokens).reverse))
            case head::tail => Partial(new ConcatentionParser(head.getParser, tail, token::tokens, None))
          }
        }
        case Partial(parser1) => Partial(new ConcatentionParser(parser1, rules, tokens, previousMatch))
        case Match(token, None) => rules match {
          case Nil => Match(Token.Concatenation((token::tokens).reverse))
          case head::tail => Partial(new ConcatentionParser(head.getParser, tail, token::tokens, previousMatch))
        }
        case Match(token, Some(parser1)) => Partial(new ConcatentionParser(parser1, rules, tokens, Some(token)))
      }

      override def rule: Rule = Concatenation.this
    }
    override def getParser: RuleParser = new ConcatentionParser(rules.head.getParser, rules.tail, Nil, None)
  }

  object OptionalSequence {
    def apply(rules: Rule*): OptionalSequence = new OptionalSequence(rules: _*)
  }

  class OptionalSequence(rules: Rule*) extends VariableRepetition(Concatenation(rules: _*), None, Some(1)) {
//    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
//      case (p, Left(Token.Repetition(s, e, List(Token.Concatenation(_, _, ts))))) =>
//        (p, Left(Token.OptionalSequence(s, e, ts)))
//      case (p, Left(Token.Repetition(s, e, Nil))) =>
//        (p, Left(Token.OptionalSequence(s, e, Nil)))
//      case (p, Right(_)) => (p, Right(this))
//    }
    override def getParser: RuleParser = ???
  }

  object SpecificRepetition {
    def apply(rule: Rule, n: Int): SpecificRepetition = new SpecificRepetition(rule, n)
  }

  class SpecificRepetition(rule: Rule, n: Int) extends VariableRepetition(rule, Some(n), Some(n))

  object VariableRepetition {
    def apply(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None): VariableRepetition = new VariableRepetition(rule, lower, upper)
  }

  class VariableRepetition(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None) extends Rule {
    override def getParser: RuleParser = ???
  }

  object Terminal {
    def apply(value: String): Terminal = new Terminal(value)
  }

  class Terminal(value: String) extends Rule {

    class TerminalParser(position: Int) extends RuleParser {
      override def parse(input: Char): ParseResult =
        if (value.charAt(position) != input) Error
        else if (position + 1 == value.length) Match(Token.Terminal(value))
        else Partial(new TerminalParser(position + 1))

      override def rule: Rule = Terminal.this
    }

    override def getParser: RuleParser = new TerminalParser(0)
  }

  object Range {
    def apply(lower: Char, upper: Char, token: Char => Token): Range = new Range(lower, upper, token)
  }

  class Range(lower: Char, upper: Char, token: Char => Token) extends Rule {
    override def getParser: RuleParser = new RuleParser {
      override def rule: Rule = Range.this

      override def parse(c: Char): ParseResult = if (c >= lower && c <= upper) Match(token(c)) else Error
    }
  }

  object Single {
    def apply(c: Char, token: Char => Token = Token.Single) = new Single(c, token)
  }

  class Single(c: Char, token: Char => Token) extends Rule {
    override def getParser: RuleParser = new RuleParser {
      override val rule = Single.this

      override def parse(c1: Char): ParseResult = if (c1 == c) Match(token(c1)) else Error
    }
  }

  // ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
  case object ALPHA extends Alternative(Range(0x41, 0x5a, Token.ALPHA), Range(0x61, 0x7a, Token.ALPHA))

  // BIT            =  "0" / "1"
  case object BIT extends Range(0x30,0x31, Token.BIT.apply)

  // CHAR           =  %x01-7F
  // any 7-bit US-ASCII character, excluding NUL
  case object CHAR extends Range(0x31, 0x7f, Token.CHAR)

  // carriage return
  case object CR extends Single(0x0d, c => Token.CR)

  // Internet standard newline
  case object CRLF extends Concatenation(CR, LF)

  //  CTL            =  %x00-1F / %x7F
  // controls
  case object CTL extends Alternative(Range(0x00, 0x1f, Token.CTL), Single(0x7f, Token.CTL))

  // 0-9
  case object DIGIT extends Range(0x30, 0x39, c => Token.DIGIT(c.toInt))

  // " (Double Quote)
  case object DQUOTE extends Single(0x22, c => Token.DQUOTE)

  case object HEXDIG extends Alternative(DIGIT, Single('A'), Single('B'), Single('C'), Single('D'), Single('E'), Single('F'))

  case object HTAB extends Single(0x09, c => Token.HTAB)

  // linefeed
  case object LF extends Single(0x0a, c => Token.LF)

  //  Use of this linear-white-space rule
  //   permits lines containing only white
  //   space that are no longer legal in
  //   mail headers and have caused
  //   interoperability problems in other
  //   contexts.
  //  Do not use when defining mail
  //   headers and use with caution in
  //   other contexts.
  case object LWSP extends VariableRepetition(Alternative(WSP, Concatenation(CRLF,WSP)))

  // 8 bits of data
  case object OCTET extends Range(0x00, 0xff, Token.OCTET)

  case object SP extends Single(0x20, c => Token.SP)

  // visible (printing) characters
  case object VCHAR extends Range(0x21, 0x7e, Token.VCHAR)

  // white space
  case object WSP extends Alternative(SP, HTAB)

}

//object Parser {
//  def parse(input: String, grammar: Rule): Either[Token, (Int, Rule)] = grammar.parse(input, 0) match {
//    case (_, Left(t)) => Left(t)
//    case (p, Right(r)) => Right((p, r))
//  }
//}
