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

  it should "fail to parse on out of bounds request" in {
    val input = "12345"
    val rule = Concatenation(ALPHA, SP)
    rule.parse(input, input.length) should equal((input.length, Right(rule)))
  }

  def parseFailures(rule: Rule, input: String): Unit = {

    it should "not parse improper input" in {
      rule.parse(input, 0) should equal((0, Right(rule)))
    }

    it should "fail to parse on out of bounds request" in {
      rule.parse(input, input.length) should equal((input.length, Right(rule)))
    }
  }

}
