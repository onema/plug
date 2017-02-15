package abnf

import abnf.Rule._
import org.scalatest.{FlatSpec, Matchers}

class RuleSpec extends FlatSpec with Matchers {

  "ALPHA" should "parse alpha from position in string" in {
    ALPHA.parse("012X45", 3) should equal(4, Left(Token.ALPHA(3)))
  }

  it should behave like parseFailures(ALPHA, "012345")

  "DIGIT" should "parse alpha from position in string" in {
    DIGIT.parse("aaa5bbb", 3) should equal(4, Left(Token.DIGIT(3)))
  }

  "CRLF" should "match \r\n linefeed" in {
    CRLF.parse("x\r\ny", 1) should equal((3, Left(Token.CRLF(1, 3))))
  }

  it should behave like parseFailures(DIGIT, "abcdef")

  "Single" should "parse single, exact char" in {
    Single('x').parse("aaaxbbb", 3) should equal(4, Left(Token.Single(3)))
  }

  it should behave like parseFailures(Single('x'), "aaaaaa")

  "Range" should "parse single char included in specified range" in {
    Range('d', 'f').parse("aaadbbb", 3) should equal((4, Left(Token.Single(3))))
    Range('d', 'f').parse("aaaebbb", 3) should equal((4, Left(Token.Single(3))))
    Range('d', 'f').parse("aaafbbb", 3) should equal((4, Left(Token.Single(3))))
  }

  it should behave like parseFailures(Range('d', 'f'), "aaaaaa")

  "Terminal" should "match on exact string" in {
    Terminal("abc").parse("xxxabcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "abc")))
  }

  it should "match on case-insensitive match" in {
    Terminal("abc").parse("xxxAbCcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "AbC")))
  }

  it should behave like parseFailures(Terminal("abc"), "aaaaaaa")

  "Alternative" should "match one of possibilities" in {
    Alternative(ALPHA, DIGIT, SP).parse("aaaxbbb", 3) should equal((4, Left(Token.ALPHA(3))))
    Alternative(ALPHA, DIGIT, SP).parse("aaa5bbb", 3) should equal((4, Left(Token.DIGIT(3))))
    Alternative(ALPHA, DIGIT, SP).parse("aaa bbb", 3) should equal((4, Left(Token.SP(3))))
  }

  it should "match possibilities from left to right" in {
    val alt = Alternative(Terminal("abcdef"), Terminal("abc"), Terminal("abcdefg"))
    alt.parse("xxxabcdefgxyz", 3) should equal(9, Left(Token.Terminal(3, 9, "abcdef")))
  }

  it should behave like parseFailures(Alternative(ALPHA, DIGIT, SP), "--------")

  "Concatenation" should "match an exact series" in {
    import Token.{ALPHA => A}
    val con = Concatenation(Terminal("Bob"), SP, VariableRepetition(ALPHA, Some(1)))
    val token = Token.Concatenation(3, 12, List(
      Token.Terminal(3, 6, "Bob"),
      Token.SP(6),
      Token.Repetition(7, 12, List(A(7), A(8), A(9), A(10), A(11)))
    ))
    con.parse("   Bob Smith   ", 3) should equal((12, Left(token)))
  }

  it should "report first part that fails match" in {
    Concatenation(ALPHA, SP).parse("...aa   ", 3) should equal((4, Right(SP)))
  }

  it should behave like parseFailures(Concatenation(ALPHA, SP), "--------", Some(ALPHA))

  "VariableRepetition" should "match match all possible without limits" in {
    import Token.{ALPHA => A}
    val rule = VariableRepetition(ALPHA)
    rule.parse("aaaabc123", 3) should equal((6, Left(Token.Repetition(3, 6, List(A(3), A(4), A(5))))))
  }

  it should "consume the rest of the string as long as it matches without limits" in {
    import Token.{ALPHA => A}
    val rule = VariableRepetition(ALPHA)
    rule.parse("aaaabc", 3) should equal((6, Left(Token.Repetition(3, 6, List(A(3), A(4), A(5))))))
  }

  it should "return an empty Repetition if nothing matches and 0 matches are allowed" in {
    val rule = VariableRepetition(ALPHA)
    rule.parse("12345", 3) should equal((3, Left(Token.Repetition(3, 3, Nil))))
  }

  it should "only match if at least as many as lower bound are matched" in {
    import Token.{ALPHA => A}
    val rule = VariableRepetition(ALPHA, Some(2))
    rule.parse("aaaab1", 3) should equal((5, Left(Token.Repetition(3, 5, List(A(3), A(4))))))
  }

  it should "not match if less than lower bound are matched" in {
    val rule = VariableRepetition(ALPHA, Some(2))
    rule.parse("123a45", 3) should equal((4, Right(ALPHA)))
  }

  it should "fail on upper bound if lower bound has not been met yet" in {
    val rule = VariableRepetition(ALPHA, Some(2))
    rule.parse("aaaa", 3) should equal((4, Right(ALPHA)))
  }

  it should "match only as many as upper bound allows" in {
    import Token.{ALPHA => A}
    val rule = VariableRepetition(ALPHA, None, Some(2))
    rule.parse("aaaabc", 3) should equal((5, Left(Token.Repetition(3, 5, List(A(3), A(4))))))
  }

  it should "not fail on bounds if repetion has no lower bound" in {
    val rule = VariableRepetition(ALPHA)
    rule.parse("aaa", 3) should equal((3, Left(Token.Repetition(3, 3, Nil))))
  }

  "OptionalSequence" should "match sequence" in {
    import Token.{ALPHA => A, DIGIT => D, SP => S}
    val rule = OptionalSequence(ALPHA, SP, DIGIT)
    rule.parse("xxxa 1yyy", 3) should equal((6, Left(Token.OptionalSequence(3, 6, List(A(3), S(4), D(5))))))
  }

  it should "match lack of a sequence" in {
    val rule = OptionalSequence(ALPHA, SP, DIGIT)
    rule.parse("xxxa ayyy", 3) should equal((3, Left(Token.OptionalSequence(3, 3, Nil))))
  }

  it should "fail to parse on out of bounds request" in {
    val rule = OptionalSequence(ALPHA, SP, DIGIT)
    rule.parse("aaa", 3) should equal((3, Left(Token.OptionalSequence(3, 3, Nil))))
  }

  "CustomRule" should "act as a pass through without overrides" in {
    val rule = new CustomRule(Terminal("abc")) {}

    rule.parse("xxxabcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "abc")))
  }

  it should "pass through if matcher misses" in {
    case class Never(start: Int, end: Int) extends Token
    val rule = new CustomRule(Terminal("abc")) {
      override def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
        case (p, Left(t@Token.Single(s))) => (p, Left(Never(s, t.end)))
      }
    }

    rule.parse("xxxabcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "abc")))
  }

  it should "allow replacement of match" in {
    case class StartsWithF(start: Int, end: Int, value: String) extends Token

    val rule = new CustomRule(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1)))) {
      override def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
        case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(StartsWithF(s, e, input.substring(s, e))))
      }
    }

    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  }

  it should "replace match" in {
    case class StartsWithF(start: Int, end: Int, value: String) extends Token

    val rule = new CustomRule2(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1))))((input, position) => {
      case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(StartsWithF(s, e, input.substring(s, e))))
    })

    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  }

  it should "intercept match" in {
    case class StartsWithF(start: Int, end: Int, value: String) extends Token

    val rule = new CustomRule3(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1)))) {
      override def intercept(input: String, start: Int, end: Int, matched: Token): Either[Token, Rule] =
        Left(StartsWithF(start, end, input.substring(start, end)))
    }

    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  }

  def parseFailures(rule: Rule, input: String, failRule: Option[Rule] = None): Unit = {

    it should "not parse improper input" in {

      rule.parse(input, 0) should equal((0, Right(failRule.getOrElse(rule))))
    }

    it should "not fail to parse on out of bounds request" in {
      rule.parse(input, input.length) should equal((input.length, Right(rule)))
    }
  }

}
