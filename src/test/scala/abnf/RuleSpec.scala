package abnf

import abnf.Rule._
import org.scalatest.{FlatSpec, Matchers}

class RuleSpec extends FlatSpec with Matchers {

  //  "ALPHA" should "parse alpha from position in string" in {
  //    ALPHA.parse("012X45", 3) should equal(4, Left(Token.ALPHA(3)))
  //  }

  //  it should behave like parseFailures(ALPHA, "012345")

  "Single" should "parse match char" in {
    Single('x').getParser.parse('x') should equal(Match(Token.Single('x')))
  }

  it should "return Error for mismatch" in {
    Single('x').getParser.parse('y') should equal(Error)
  }

  "Range" should "parse single char included in specified range" in {
    Range('d', 'f').getParser.parse('d') should equal(Match(Token.Single('d')))
    Range('d', 'f').getParser.parse('f') should equal(Match(Token.Single('f')))
  }

  it should "return Error for mismatch" in {
    Range('d', 'f').getParser.parse('x') should equal(Error)
  }

  "Terminal" should "match on exact string" in
    parseString(Terminal("abc"), "abc", List(P, P, M(Token.Terminal("abc"))))

  it should "match case-insensitively" in
    parseString(Terminal("abc"), "aBc", List(P, P, M(Token.Terminal("abc"))))

  it should "match partials until failure from abc to abx" in
    parseString(Terminal("abc"), "abx", List(P, P, E))

  it should "match partials until failure from abc to axc" in
    parseString(Terminal("abc"), "axc", List(P, E))

  "Concatention" should "match an exact series" in
    parseString(
      Concatenation(Terminal("bob"), SP, Terminal("smith")),
      "Bob Smith",
      List(P, P, M(Token.Concatenation(List(Token.Terminal("bob"), Token.SP, Token.Terminal("smith"))))))

  //  "Concatenation" should "match an exact series" in {
  //    import Token.{ALPHA => A}
  //    val con = Concatenation(Terminal("Bob"), SP, VariableRepetition(ALPHA, Some(1)))
  //    val token = Token.Concatenation(3, 12, List(
  //      Token.Terminal(3, 6, "Bob"),
  //      Token.SP(6),
  //      Token.Repetition(7, 12, List(A(7), A(8), A(9), A(10), A(11)))
  //    ))
  //    con.parse("   Bob Smith   ", 3) should equal((12, Left(token)))
  //  }

  //  it should "report first part that fails match" in {
  //    Concatenation(ALPHA, SP).parse("...aa   ", 3) should equal((4, Right(SP)))
  //  }
  //
  //  it should behave like parseFailures(Concatenation(ALPHA, SP), "--------", Some(ALPHA))

  "DIGIT" should "parse number from char" in {
    DIGIT.getParser.parse('5') should equal(Match(Token.DIGIT(5)))
  }

  "CRLF" should "return partial for \r and then match for \n" in
    parseString(CRLF, "\r\n", List(P, M(Token.CRLF)))

  //  "Alternative" should "match one of possibilities" in {
  //    Alternative(ALPHA, DIGIT, SP).parse("aaaxbbb", 3) should equal((4, Left(Token.ALPHA(3))))
  //    Alternative(ALPHA, DIGIT, SP).parse("aaa5bbb", 3) should equal((4, Left(Token.DIGIT(3))))
  //    Alternative(ALPHA, DIGIT, SP).parse("aaa bbb", 3) should equal((4, Left(Token.SP(3))))
  //  }
  //
  //  it should "match possibilities from left to right" in {
  //    val alt = Alternative(Terminal("xyz"), Terminal("abcdef"), Terminal("abc"), Terminal("abcdefg"))
  //    alt.parse("xxxabcdefgxyz", 3) should equal(9, Left(Token.Terminal(3, 9, "abcdef")))
  //  }
  //
  //  it should behave like parseFailures(Alternative(ALPHA, DIGIT, SP), "--------")


  //  "VariableRepetition" should "match match all possible without limits" in {
  //    import Token.{ALPHA => A}
  //    val rule = VariableRepetition(ALPHA)
  //    rule.parse("aaaabc123", 3) should equal((6, Left(Token.Repetition(3, 6, List(A(3), A(4), A(5))))))
  //  }
  //
  //  it should "consume the rest of the string as long as it matches without limits" in {
  //    import Token.{ALPHA => A}
  //    val rule = VariableRepetition(ALPHA)
  //    rule.parse("aaaabc", 3) should equal((6, Left(Token.Repetition(3, 6, List(A(3), A(4), A(5))))))
  //  }
  //
  //  it should "return an empty Repetition if nothing matches and 0 matches are allowed" in {
  //    val rule = VariableRepetition(ALPHA)
  //    rule.parse("12345", 3) should equal((3, Left(Token.Repetition(3, 3, Nil))))
  //  }
  //
  //  it should "only match if at least as many as lower bound are matched" in {
  //    import Token.{ALPHA => A}
  //    val rule = VariableRepetition(ALPHA, Some(2))
  //    rule.parse("aaaab1", 3) should equal((5, Left(Token.Repetition(3, 5, List(A(3), A(4))))))
  //  }
  //
  //  it should "not match if less than lower bound are matched" in {
  //    val rule = VariableRepetition(ALPHA, Some(2))
  //    rule.parse("123a45", 3) should equal((4, Right(ALPHA)))
  //  }
  //
  //  it should "fail on upper bound if lower bound has not been met yet" in {
  //    val rule = VariableRepetition(ALPHA, Some(2))
  //    rule.parse("aaaa", 3) should equal((4, Right(ALPHA)))
  //  }
  //
  //  it should "match only as many as upper bound allows" in {
  //    import Token.{ALPHA => A}
  //    val rule = VariableRepetition(ALPHA, None, Some(2))
  //    rule.parse("aaaabc", 3) should equal((5, Left(Token.Repetition(3, 5, List(A(3), A(4))))))
  //  }
  //
  //  it should "not fail on bounds if repetion has no lower bound" in {
  //    val rule = VariableRepetition(ALPHA)
  //    rule.parse("aaa", 3) should equal((3, Left(Token.Repetition(3, 3, Nil))))
  //  }
  //
  //  "OptionalSequence" should "match sequence" in {
  //    import Token.{ALPHA => A, DIGIT => D, SP => S}
  //    val rule = OptionalSequence(ALPHA, SP, DIGIT)
  //    rule.parse("xxxa 1yyy", 3) should equal((6, Left(Token.OptionalSequence(3, 6, List(A(3), S(4), D(5))))))
  //  }
  //
  //  it should "match lack of a sequence" in {
  //    val rule = OptionalSequence(ALPHA, SP, DIGIT)
  //    rule.parse("xxxa ayyy", 3) should equal((3, Left(Token.OptionalSequence(3, 3, Nil))))
  //  }
  //
  //  it should "fail to parse on out of bounds request" in {
  //    val rule = OptionalSequence(ALPHA, SP, DIGIT)
  //    rule.parse("aaa", 3) should equal((3, Left(Token.OptionalSequence(3, 3, Nil))))
  //  }
  //
  //  "CustomRule" should "act as a pass through without overrides" in {
  //    val rule = new CustomRule(Terminal("abc")) {}
  //
  //    rule.parse("xxxabcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "abc")))
  //  }
  //
  //  it should "pass through if matcher misses" in {
  //    case class Never(start: Int, end: Int) extends Token
  //    val rule = new CustomRule(Terminal("abc")) {
  //      override def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
  //        case (p, Left(t@Token.Single(s))) => (p, Left(Never(s, t.end)))
  //      }
  //    }
  //
  //    rule.parse("xxxabcyyy", 3) should equal(6, Left(Token.Terminal(3, 6, "abc")))
  //  }
  //
  //  it should "allow replacement of match" in {
  //    case class StartsWithF(start: Int, end: Int, value: String) extends Token
  //
  //    val rule = new CustomRule(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1)))) {
  //      override def matcher(input: String, position: Int): PartialFunction[(Int, Either[Token, Rule]), (Int, Either[Token, Rule])] = {
  //        case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(StartsWithF(s, e, input.substring(s, e))))
  //      }
  //    }
  //
  //    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  //  }
  //
  //  it should "replace match" in {
  //    case class StartsWithF(start: Int, end: Int, value: String) extends Token
  //
  //    val rule = new CustomRule2(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1))))((input, position) => {
  //      case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(StartsWithF(s, e, input.substring(s, e))))
  //    })
  //
  //    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  //  }
  //
  //  it should "intercept match" in {
  //    case class StartsWithF(start: Int, end: Int, value: String) extends Token
  //
  //    val rule = new CustomRule3(Concatenation(Single('F'), VariableRepetition(ALPHA, Some(1)))) {
  //      override def intercept(input: String, start: Int, end: Int, matched: Token): Either[Token, Rule] =
  //        Left(StartsWithF(start, end, input.substring(start, end)))
  //    }
  //
  //    rule.parse("Foobar", 0) should equal(6, Left(StartsWithF(0, 6, "Foobar")))
  //  }

  def parseString(rule: Rule, input: String, expected: List[ParseTest]): Unit = {
    def parse(stream: List[Char], parser: RuleParser, expected2: List[(ParseTest,Int)]): Unit = stream match {
      case Nil =>
      case head :: tail =>
        val (ehead,idx) :: etail = expected2
        parser.parse(head) match {
          case Partial(p) =>
            if (ehead == P) parse(tail, p, etail)
            else fail(s"$idx: $ehead != P")
          case Match(token, None) =>
            if (ehead != M(token)) fail(s"$ehead != ${M(token)}")
            else if (etail.nonEmpty) fail(s"$idx: Final match on $ehead, but expected not empty: $etail")
          case Match(token, Some(p)) =>
            if (ehead != MC(token)) fail(s"$ehead != ${MC(token)}")
            else if (etail.isEmpty) fail(s"$idx: Non-final match on $ehead, but no more expected")
            else parse(tail, p, etail)
          case Error => if (ehead != E) fail(s"$idx: $ehead != E")
        }
    }
    parse(input.toList, rule.getParser, expected.zipWithIndex)
  }

}

sealed trait ParseTest

case object P extends ParseTest

case object E extends ParseTest

case class M(token: Token) extends ParseTest

case class MC(token: Token) extends ParseTest


