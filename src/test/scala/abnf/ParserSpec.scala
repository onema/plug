package abnf

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  "parse" should "return a Token tree for postal address" in {
    val address ="Joe F. Smith\r\n123 Main Street Apt 5a\r\nSan Diego, CA 92101-1234\r\n"
    val tokens = Parser.parse(address, PostalAddressGrammar.PostalAddress)
    println(tokens)
  }

  it should "parse State" in {
    import PostalAddressTokens._
    val expected = Left(State(0, 2, "CA"))
    val actual = Parser.parse("CA", PostalAddressGrammar.State)

    actual should equal(expected)
  }

  it should "parse 5 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Left(ZipCode(0, 5, 90210, None))
    val actual = Parser.parse("90210", PostalAddressGrammar.ZipCode)

    actual should equal(expected)
  }

  it should "parse 5+4 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Left(ZipCode(0, 10, 90210, Some(1234)))
    val actual = Parser.parse("90210-1234", PostalAddressGrammar.ZipCode)

    actual should equal(expected)
  }

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
    case object FirstName extends VariableRepetition(ALPHA, Some(1)) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.FirstName(s, e, input.substring(s, e))))
        case x => x
      }
    }

    // MiddleName = 1*(ALPHA)
    case object MiddleName extends VariableRepetition(ALPHA, Some(1)) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.MiddleName(s, e, input.substring(s, e))))
        case x => x
      }
    }

    // Initial = ALPHA "."
    case object Initial extends Concatenation(ALPHA, Terminal(".")) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Concatenation(_, _, List(a, _)))) =>
          (p, Left(PostalAddressTokens.Initial(a.start, a.end, input.substring(a.start, a.end))))
        case x => x
      }
    }

    // LastName = 1*(ALPHA)
    case object LastName extends VariableRepetition(ALPHA, Some(1)) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.LastName(s, e, input.substring(s, e))))
        case x => x
      }
    }

    // Suffix = "Jr." / "Sr." / 1*("I" / "V" / "X")
    case object Suffix extends Alternative(
      Terminal("Jr."),
      Terminal("Sr."),
      VariableRepetition(Alternative(Terminal("I"), Terminal("V"), Terminal("X")), Some(1))
    ) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Terminal(s, e, v))) => (p, Left(PostalAddressTokens.Suffix(s, e, v)))
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.Suffix(s, e, input.substring(s, e))))
        case x => x
      }
    }

    // Street = [HouseNum SP] StreetName AptOrSuite CRLF
    case object Street extends Concatenation(
      OptionalSequence(HouseNum, SP),
      StreetName,
      AptOrSuite,
      CRLF
    ) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        // Note: substring is the Concat end -2 to cut off the CRLF
        case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(PostalAddressTokens.Street(s, e, input.substring(s, e - 2))))
        case x => x
      }
    }


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
    case object TownName extends VariableRepetition(Alternative(ALPHA, SP), Some(1)) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.TownName(s, e, input.substring(s, e))))
        case x => x
      }
    }


    // State = 2(ALPHA)
    case object State extends SpecificRepetition(ALPHA, 2) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token(start, end))) => (p, Left(PostalAddressTokens.State(start, end, input.substring(start, end))))
        case x => x
      }
    }

    // ZipCode = 5(DIGIT) ["-" 4(DIGIT)]
    case object ZipCode extends Concatenation(
      SpecificRepetition(DIGIT, 5),
      OptionalSequence(Terminal("-"), SpecificRepetition(DIGIT, 4))
    ) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Concatenation(s, e, List(Token.Repetition(start, end, _), Token.OptionalSequence(_, _, Nil))))) =>
          (p, Left(PostalAddressTokens.ZipCode(s, e, input.substring(start, end).toInt, None)))
        case (p, Left(Token.Concatenation(s, e, List(
        Token.Repetition(start, end, _),
        Token.OptionalSequence(_, _, List(_, Token.Repetition(start1, end1, _)))
        )))) =>
          (p, Left(PostalAddressTokens.ZipCode(s, e, input.substring(start, end).toInt, Some(input.substring(start1, end1).toInt))))
        case x => x
      }
    }

  }

  object PostalAddressTokens {

    case class FirstName(start: Int, end: Int, value: String) extends Token

    case class LastName(start: Int, end: Int, value: String) extends Token

    case class MiddleName(start: Int, end: Int, value: String) extends Token

    case class Initial(start: Int, end: Int, value: String) extends Token

    case class Suffix(start: Int, end: Int, value: String) extends Token

    case class Street(start: Int, end: Int, value: String) extends Token

    case class TownName(start: Int, end: Int, value: String) extends Token

    case class State(start: Int, end: Int, code: String) extends Token

    case class ZipCode(start: Int, end: Int, zip: Int, plus4: Option[Int]) extends Token

  }

}
