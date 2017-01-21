package abnf

trait Token {
  def start: Int

  def end: Int
}

object Token {

  def unapply(arg: Token): Option[(Int, Int)] = Some(arg.start,arg.end)

  trait Value extends Token {
    val start: Int

    def end: Int = start
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

}

trait Rule {
  // TODO: Instead of Option[(Int,Token)] consider (Int,Either[Token,Rule]) to communicate where it failed and on what
  def parse(input: String, position: Int): (Int, Either[Token,Rule]) =
    if(position >= input.length) (position,Right(Rule.OOB))
    else safeParse(input, position)

  def safeParse(input: String, position: Int): (Int, Either[Token,Rule])
}

object Rule {

  case object OOB extends Rule {
    override def safeParse(input: String, position: Int): (Int, Either[Token, Rule]) = ???
  }

  object Alternative {
    def apply(rules: Rule*): Alternative = new Alternative(rules: _*)
  }

  implicit val defaultToken: Option[(Token => Token)] = None

  class Alternative(rules: Rule*) extends Rule {
    override def safeParse(input: String, position: Int): Option[(Int, Token)] = {
      def tryParse(rules: List[Rule]): Option[(Int, Token)] = rules match {
        case Nil => None
        case head :: tail => head.parse(input, position) match {
          case None => tryParse(tail)
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
    override def safeParse(input: String, position: Int): Option[(Int, Token)] = {
      def parse(rules: List[Rule], p: Int, r: List[Token]): Option[(Int, Token)] = rules match {
        case Nil =>
          val defaultToken = Token.Concatenation(position, p-1, r.reverse)
          Some((p, token match {
            case None => defaultToken
            case Some(f) => f(defaultToken)
          }))
        case head :: tail => head.parse(input, p) match {
          case None => None
          case Some((p2, t)) => parse(tail, p2, t :: r)
        }
      }
      parse(rules.toList, position, Nil)
    }
  }

  object OptionalSequence {
    def apply(rules: Rule*): OptionalSequence = new OptionalSequence(rules: _*)
  }

  class OptionalSequence(rules: Rule*) extends VariableRepetition(Concatenation(rules: _*), None, Some(1))

  object SpecificRepetition {
    def apply(rule: Rule, n: Int): SpecificRepetition = new SpecificRepetition(rule, n)
  }

  class SpecificRepetition(rule: Rule, n: Int) extends VariableRepetition(rule, Some(n), Some(n))

  object VariableRepetition {
    def apply(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None): VariableRepetition = new VariableRepetition(rule, lower, upper)
  }

  class VariableRepetition(rule: Rule, lower: Option[Int] = None, upper: Option[Int] = None) extends Rule {
    override def safeParse(input: String, position: Int): Option[(Int, Token)] = {
      val l = lower.getOrElse(0)
      val u = upper.getOrElse(Int.MaxValue)
      if (l < 0 || l > u || u == 0)
        None
      else {
        import Token.Repetition
        def parse(i: Int, p: Int, r: List[Token]): Option[(Int, Token)] = {
          if (i >= u) {
            Some((p, Repetition(position, p-1, r.reverse)))
          } else {
            rule.parse(input, p) match {
              case None => if (r.length >= l) Some((p, Repetition(position, p-1, r.reverse))) else None
              case Some((p2, t)) => parse(i + 1, p2, t :: r)
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
    override def safeParse(input: String, position: Int): Option[(Int, Token)] = {
      val l = value.length
      if (input.length - position < l) None
      else if (input.substring(position, position + l).equalsIgnoreCase(value))
        Some((position + l+1, Token.Terminal(position, position + l, value)))
      else None
    }
  }

  object Range {
    def apply(lower: Char, upper: Char, token: Int => Token = Token.Single): Range = new Range(lower, upper, token)
  }

  class Range(lower: Char, upper: Char, token: Int => Token) extends Rule {
    override def safeParse(input: String, position: Int): Option[(Int, Token)] = {
      val c = input(position)
      if (c >= lower && c <= upper) Some((position + 1, token(position))) else None
    }
  }

  object Single {
    def apply(c: Char): Single = new Single(c, Token.Single)
  }

  class Single(c: Char, token: Int => Token) extends Rule {
    override def safeParse(input: String, position: Int): Option[(Int, Token)] =
      if (input(position) == c) Some((position + 1, token(position))) else None
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
  def parse(input: String, grammar: Rule): Option[Token] = grammar.parse(input, 0).map(_._2)
}
