package abnf

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  "parse" should "return a Token tree for postal address" in {
    import PostalAddressTokens._
    val address =
      """Joe F. Smith III
        |123 Main Street Apt 5a
        |San Diego, CA 92101-1234
      """.stripMargin
    val expected = Left(Token.Concatenation(1, 2, List(
      Token.Concatenation(1, 12, List(
        FirstName(1, 2, "Joe"), Token.SP(3),
        Initial(4, 5, "F"), Token.SP(7),
        LastName(8, 9, "Smith"), Token.SP(10),
        Suffix(11, 12, "III")
      )),
      Street(8, 9, "123 Main Street Apt 5a"),
      Token.Concatenation(10, 11, List(
        TownName(10, 11, "San Diego"), Token.Terminal(12, 13, ","),
        State(14, 16, "CA"), Token.SP(17),
        ZipCode(17, 18, 92101, Some(1234))
      ))
    )))
    val actual = Parser.parse(address, PostalAddressGrammar.PostalAddress)
    actual should equal(expected)
  }

  it should "parse NamePart" in {
    import PostalAddressTokens._
    val expected = Left(Token.Concatenation(1, 12, List(
      FirstName(1, 2, "Joe"), Token.SP(3),
      Initial(4, 5, "F"), Token.SP(7),
      LastName(8, 9, "Smith"), Token.SP(10),
      Suffix(11, 12, "III")
    )))
    val actual = Parser.parse("Joe F. Smith III", PostalAddressGrammar.NamePart)
    actual should equal(expected)
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

  object PostalGrammar {

    import Rule._

    //postal-address   = name-part street zip-part
    case object PostalAddress extends Concatenation(NamePart, Street, ZipPart)

    //name-part        = *(personal-part SP) last-name [SP suffix] newline
    //name-part        =/ personal-part newline
    case object NamePart extends Alternative(
      Concatenation(
        VariableRepetition(Concatenation(PersonalPart, SP)),
        LastName,
        OptionalSequence(SP, Suffix),
        NewLine
      ),
      Concatenation(PersonalPart, NewLine)
    )

    //personal-part    = first-name / initial
    case object PersonalPart extends Alternative(FirstName, Initial)

    //first-name       = *ALPHA
    case object FirstName extends VariableRepetition(ALPHA)

    // initial          = ALPHA "."
    case object Initial extends Concatenation(ALPHA, Terminal("."))

    //last-name        = *ALPHA
    case object LastName extends VariableRepetition(ALPHA)

    //suffix           = ("Jr." / "Sr." / 1*("I" / "V" / "X"))
    case object Suffix extends Alternative(
      Terminal("Jr."),
      Terminal("Sr."),
      VariableRepetition(Alternative(Terminal("I"), Terminal("V"), Terminal("X")), Some(1))
    )

    //street           = [apt SP] house-num SP street-name newline
    case object Street extends Concatenation(
      OptionalSequence(Apt, SP),
      HouseNum,
      SP,
      StreetName,
      NewLine
    )

    //apt              = 1*4DIGIT
    case object Apt extends VariableRepetition(DIGIT, Some(1), Some(4))

    //house-num        = 1*8(DIGIT / ALPHA)
    case object HouseNum extends VariableRepetition(Alternative(DIGIT, ALPHA), Some(1), Some(8))

    //street-name      = 1*VCHAR
    case object StreetName extends VariableRepetition(VCHAR, Some(1))

    //zip-part         = town-name "," SP state 1*2SP zip-code newline
    case object ZipPart extends Concatenation(
      TownName,
      Terminal(","),
      SP,
      State,
      VariableRepetition(SP, Some(1), Some(2)),
      ZipCode,
      NewLine)

    //town-name        = 1*(ALPHA / SP)
    case object TownName extends VariableRepetition(Alternative(ALPHA, SP), Some(1))

    //state            = 2ALPHA
    case object State extends SpecificRepetition(ALPHA, 2)

    //zip-code         = 5DIGIT ["-" 4DIGIT]
    case object ZipCode extends Concatenation(
      SpecificRepetition(DIGIT, 5),
      OptionalSequence(Terminal("-"), SpecificRepetition(DIGIT, 4))
    )

    //newline          = CRLF / LF
    case object NewLine extends Alternative(CRLF, LF) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.CRLF(s, e))) => (p, Left(PostalAddressTokens.NewLine(s, e)))
        case (p, Left(lf: Token.LF)) => (p, Left(PostalAddressTokens.NewLine(lf.start, lf.end)))
        case x => x
      }
    }

  }

  object PostalAddressGrammar {

    import Rule._

    // PostalAddress = NamePart NewLine Street NewLine ZipPart
    case object PostalAddress extends Concatenation(NamePart, NewLine, Street, NewLine, ZipPart)

    case object NewLine extends Alternative(CRLF, LF) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.CRLF(s, e))) => (p, Left(PostalAddressTokens.NewLine(s, e)))
        case (p, Left(lf: Token.LF)) => (p, Left(PostalAddressTokens.NewLine(lf.start, lf.end)))
        case x => x
      }
    }


    // NamePart = LastName / (FirstName LastName [SP Suffix])
    // NamePart =/ FirstName 1*((Initial / MiddleName) SP) LastName [SP Suffix]
    case object NamePart extends Alternative(
      Concatenation(FirstName, SP, LastName, OptionalSequence(SP, Suffix)),
      LastName
    )

    // FirstName = 1*(ALPHA)
    case object FirstName extends VariableRepetition(ALPHA, Some(1)) {
      override def parse(input: String, position: Int): (Int, Either[Token, Rule]) = super.parse(input, position) match {
        case (p, Left(Token.Repetition(s, e, _))) => (p, Left(PostalAddressTokens.FirstName(s, e, input.substring(s, e))))
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

    // Street = [HouseNum SP] StreetName AptOrSuite
    case object Street extends CustomRule(Concatenation(
      OptionalSequence(HouseNum, SP),
      StreetName,
      AptOrSuite
    )) {
      override def matcher(input: String, position: Int) = {
        // Note: substring is the Concat end -2 to cut off the CRLF
        case (p, Left(Token.Concatenation(s, e, _))) => (p, Left(PostalAddressTokens.Street(s, e, input.substring(s, e - 2))))
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

    // ZipPart = TownName "," SP State SP ZipCode
    case object ZipPart extends Concatenation(TownName, Terminal(","), SP, State, SP, ZipCode)

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

    case class NewLine(start: Int, end: Int) extends Token

    case class FirstName(start: Int, end: Int, value: String) extends Token

    case class LastName(start: Int, end: Int, value: String) extends Token

    case class Initial(start: Int, end: Int, value: String) extends Token

    case class Suffix(start: Int, end: Int, value: String) extends Token

    case class Street(start: Int, end: Int, value: String) extends Token

    case class TownName(start: Int, end: Int, value: String) extends Token

    case class State(start: Int, end: Int, code: String) extends Token

    case class ZipCode(start: Int, end: Int, zip: Int, plus4: Option[Int]) extends Token

  }
}
