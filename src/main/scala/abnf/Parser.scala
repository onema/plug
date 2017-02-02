package abnf

trait Token {
  /** Inclusive start offset of match. */
  def start: Int

  /** Exclusive end offset of match. */
  def end: Int
}

object Token {

  def unapply(arg: Token): Option[(Int, Int)] = Some(arg.start, arg.end)

  trait Value extends Token {
    val start: Int

    def end: Int = start + 1
  }

  case class Single(start: Int) extends Value

  case class ALPHA(start: Int) extends Value

  case class DIGIT(start: Int) extends Value

  case class OCTET(start: Int) extends Value

  case class SP(start: Int) extends Value

  case class CR(start: Int) extends Value

  case class LF(start: Int) extends Value

  case class CRLF(start: Int, end: Int) extends Token

  case class VCHAR(start: Int) extends Value

  case class Terminal(start: Int, end: Int, value: String) extends Token

  case class Repetition(start: Int, end: Int, tokens: List[Token]) extends Token

  case class Concatenation(start: Int, end: Int, tokens: List[Token]) extends Token

  case class OptionalSequence(start: Int, end: Int, tokens: List[Token]) extends Token

}

trait Rule {

  // TODO: This should have an end as well, so you can parse in a fixed substring
  def parse(input: String, position: Int): (Int, Either[Token, Rule])

  def checkBounds(input: String, position: Int, rule: Rule)(f: => (Int, Either[Token, Rule])) =
    if (position >= input.length) (position, Right(rule))
    else f

}

class CustomRule(wrapped: Rule) extends Rule {
  def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] =
    new PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] {
      override def isDefinedAt(x: (Int, Either[Token, Rule])): Boolean = false

      override def apply(v1: (Int, Either[Token, Rule])): (Int, Either[Token, Rule]) = v1
    }

  val default: PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = { case x => x }
  override def parse(input: String, position: Int): (Int, Either[Token, Rule]) =
    (matcher(input, position) orElse default) (wrapped.parse(input, position))

}

object Rule {

  object Alternative {
    def apply(rules: Rule*): Alternative = new Alternative(rules: _*)
  }

  implicit val defaultToken: Option[(Token => Token)] = None

  class Alternative(rules: Rule*) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = checkBounds(input, position, this) {
      def tryParse(rules: List[Rule]): (Int, Either[Token, Rule]) = rules match {
        case Nil => (position, Right(this))
        case head :: tail => head.parse(input, position) match {
          case (_, Right(_)) => tryParse(tail)
          case x => x
        }
      }
      tryParse(rules.toList)
    }
  }

  object Concatenation {
    def apply(rules: Rule*): Concatenation = new Concatenation(rules: _*)

    def apply(rules: List[Rule], token: Option[(Token => Token)]): Concatenation = new Concatenation(rules: _*)(token)
  }


  class Concatenation(rules: Rule*)(implicit token: Option[(Token => Token)] = None) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = checkBounds(input, position, this) {
      def parse(rules: List[Rule], p: Int, r: List[Token]): (Int, Either[Token, Rule]) = rules match {
        case Nil =>
          val defaultToken = Token.Concatenation(position, p, r.reverse)
          (p, Left(token match {
            case None => defaultToken
            case Some(f) => f(defaultToken)
          }))
        case head :: tail => head.parse(input, p) match {
          case f@(_, Right(_)) => f
          case (p2, Left(t)) => parse(tail, p2, t :: r)
        }
      }
      parse(rules.toList, position, Nil)
    }
  }

  object OptionalSequence {
    def apply(rules: Rule*): OptionalSequence = new OptionalSequence(rules: _*)
  }

  class OptionalSequence(rules: Rule*) extends VariableRepetition(Concatenation(rules: _*), None, Some(1)) {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
      case (p, Left(Token.Repetition(s, e, List(Token.Concatenation(_, _, ts))))) =>
        (p, Left(Token.OptionalSequence(s, e, ts)))
      case (p, Left(Token.Repetition(s, e, Nil))) =>
        (p, Left(Token.OptionalSequence(s, e, Nil)))
      case (p, Right(_)) => (p, Right(this))
    }
  }

  object SpecificRepetition {
    def apply(rule: Rule, n: Int): SpecificRepetition = new SpecificRepetition(rule, n)
  }

  class SpecificRepetition(rule: Rule, n: Int) extends VariableRepetition(rule, Some(n), Some(n))

  object VariableRepetition {
    def apply(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None): VariableRepetition = new VariableRepetition(rule, lower, upper)
  }

  class VariableRepetition(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = {

      // Note: Unlike other Rules, a Repetition that is allowed to match 0 times will return Left() on hitting input
      // bounds
      val l = lower.getOrElse(0)
      val u = upper.getOrElse(Int.MaxValue)
      if (l < 0 || l > u || u == 0)

      // Note: Should there be a different way to signal that the rule's definition is invalid?
        (position, Right(this))
      else {
        import Token.Repetition
        def parse(i: Int, p: Int, r: List[Token]): (Int, Either[Token, Rule]) = {
          if (p >= input.length) {
            if (r.nonEmpty && r.length >= l) (p, Left(Repetition(position, p, r.reverse)))
            else if (l == 0) (position, Left(Repetition(position, position, Nil)))
            else (p, Right(rule))
          } else if (i >= u) {
            (p, Left(Repetition(position, p, r.reverse)))
          } else {
            rule.parse(input, p) match {
              case f@(_, Right(_)) => if (r.length >= l) (p, Left(Repetition(position, p, r.reverse))) else f
              case (p2, Left(t)) => parse(i + 1, p2, t :: r)
            }
          }
        }
        parse(0, position, Nil)
      }
    }

  }

  object Terminal {
    def apply(value: String): Terminal = new Terminal(value)
  }

  class Terminal(value: String) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = checkBounds(input, position, this) {
      val l = value.length
      if (input.length - position < l) (position, Right(this))
      else {
        val target = input.substring(position, position + l)
        if (target.equalsIgnoreCase(value))
          (position + l, Left(Token.Terminal(position, position + l, target)))
        else (position, Right(this))
      }
    }
  }

  object Range {
    def apply(lower: Char, upper: Char, token: Int => Token = Token.Single): Range = new Range(lower, upper, token)
  }

  class Range(lower: Char, upper: Char, token: Int => Token) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = checkBounds(input, position, this) {
      val c = input(position)
      if (c >= lower && c <= upper) (position + 1, Left(token(position))) else (position, Right(this))
    }
  }

  object Single {
    def apply(c: Char): Single = new Single(c, Token.Single)
  }

  class Single(c: Char, token: Int => Token) extends Rule {
    override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = checkBounds(input, position, this) {
      if (input(position) == c) (position + 1, Left(token(position))) else (position, Right(this))
    }
  }

  case object ALPHA extends Alternative(Range(0x41, 0x5a, Token.ALPHA), Range(0x61, 0x7a, Token.ALPHA))

  case object DIGIT extends Range(0x30, 0x39, Token.DIGIT)

  case object OCTET extends Range(0x00, 0xff, Token.DIGIT)

  case object SP extends Single(0x20, Token.SP)

  case object CR extends Single(0x0d, Token.CR)

  case object LF extends Single(0x0a, Token.LF)

  case object CRLF extends Concatenation(CR, LF)(Some(t => Token.CRLF(t.start, t.end)))

  case object VCHAR extends Range(0x21, 0x7e, Token.VCHAR)

}

object Parser {
  def parse(input: String, grammar: Rule): Either[Token, (Int, Rule)] = grammar.parse(input, 0) match {
    case (_, Left(t)) => Left(t)
    case (p, Right(r)) => Right((p, r))
  }
}
