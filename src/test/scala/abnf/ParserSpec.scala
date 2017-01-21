package abnf

import org.scalatest.{FlatSpec, Matchers, Spec}

class ParserSpec extends FlatSpec with Matchers {

  object PostalAddressGrammar {

    import Rule._

    // PostalAddress = NamePart Street ZipPart
    case object PostalAddress extends Concatenation(NamePart, Street, ZipPart)

    // NamePart = (LastName CRLF) / (FirstName LastName [SP Suffix] CRLF)
    // NamePart =/ FirstName 1*((Initial / MiddleName) SP) LastName [SP Suffix] CRLF
    case object NamePart extends Alternative(
      Concatenation(LastName, CRLF),
      Concatenation(FirstName, SP, LastName, OptionalSequence(SP, Suffix), CRLF),
      Concatenation(
        FirstName, SP,
        VariableRepetition(Concatenation(Alternative(Initial, MiddleName), SP), Some(1)),
        LastName,
        OptionalSequence(SP, Suffix), CRLF)
    )

    // FirstName = 1*(ALPHA)
    case object FirstName extends VariableRepetition(ALPHA, Some(1))

    // MiddleName = 1*(ALPHA)
    case object MiddleName extends VariableRepetition(ALPHA, Some(1))

    // Initial = ALPHA "."
    case object Initial extends Concatenation(ALPHA, Terminal("."))

    // LastName = 1*(ALPHA)
    case object LastName extends VariableRepetition(ALPHA, Some(1))

    // Suffix = "Jr." / "Sr." / 1*("I" / "V" / "X")
    case object Suffix extends Alternative(
      Terminal("Jr."),
      Terminal("Sr."),
      VariableRepetition(Alternative(Terminal("I"), Terminal("V"), Terminal("X")), Some(1))
    )

    // Street = [HouseNum SP] StreetName AptOrSuite CRLF
    case object Street extends Concatenation(
      OptionalSequence(HouseNum, SP),
      StreetName,
      AptOrSuite,
      CRLF
    )

    // AptOrSuite = [SP ("Suite" / "Apt.") SP 1*(DIGIT / ALPHA)]
    case object AptOrSuite extends OptionalSequence(
      SP,
      Alternative(Terminal("Suite"), Terminal("Apt.")),
      SP,
      VariableRepetition(Alternative(DIGIT, ALPHA), Some(1))
    )

    // HouseNum = 1*8(DIGIT / ALPHA)
    case object HouseNum extends VariableRepetition(Alternative(DIGIT, ALPHA), Some(1), Some(8))

    // StreetName = 1*(VCHAR)
    case object StreetName extends VariableRepetition(VCHAR, Some(1))

    // ZipPart = TownName "," SP State SP ZipCode CRLF
    case object ZipPart extends Concatenation(TownName, Terminal(","), SP, State, SP, ZipCode, CRLF)

    // TownName = 1*(ALPHA / SP)
    case object TownName extends VariableRepetition(Alternative(ALPHA, SP), Some(1))

    // State = 2(ALPHA)
    case object State extends SpecificRepetition(ALPHA, 2) {
      override def parse(input: String, position: Int): Option[(Int, Token)] = super.parse(input, position) map {
        case (p, Token(start, end)) => (p, PostalAddressTokens.State(start, end))
      }
    }

    // ZipCode = 5(DIGIT) ["-" 4(DIGIT)]
    case object ZipCode extends Concatenation(
      SpecificRepetition(DIGIT, 5),
      OptionalSequence(Terminal("-"), SpecificRepetition(DIGIT, 4))
    ) {
      override def parse(input: String, position: Int): Option[(Int, Token)] = super.parse(input, position) map {
        case (p, Token.Concatenation(s, e, List(Token.Repetition(start, end, _), Token.Repetition(_, _, Nil)))) =>
          (p, PostalAddressTokens.ZipCode(s, e, (start, end), None))
        case (p, Token.Concatenation(s, e, List(
        Token.Repetition(start, end, _),
        Token.Repetition(_, _, List(Token.Concatenation(_, _, List(_, Token.Repetition(start1, end1, _)))))
        ))) =>
          (p, PostalAddressTokens.ZipCode(s, e, (start, end), Some((start1, end1))))
      }
    }

  }

  object PostalAddressTokens {

    case class State(start: Int, end: Int) extends Token

    case class ZipCode(start: Int, end: Int, main: (Int, Int), plus4: Option[(Int, Int)]) extends Token

  }

  "parse" should "return a Token tree for postal address" in {
    val address =
      """Joe F. Smith
        |123 5a Main
        |San Diego, CA 92101-1234
      """.stripMargin
    val tokens = Parser.parse(address, PostalAddressGrammar.PostalAddress)
    println(tokens)
  }

  it should "parse State" in {
    import PostalAddressTokens._
    val expected = Some(State(0, 1))
    val actual = Parser.parse("CA", PostalAddressGrammar.State)

    actual should equal(expected)
  }

  it should "parse 5 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Some(ZipCode(0,4,(0, 4), None))
    val actual = Parser.parse("90210", PostalAddressGrammar.ZipCode)
  }

  it should "parse 5+4 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Some(ZipCode(0,9,(0,4),Some((6, 9))))
    val actual = Parser.parse("90210-1234", PostalAddressGrammar.ZipCode)
  }
}
