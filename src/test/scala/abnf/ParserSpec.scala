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
      Name(3, 4, "Joe F. Smith III"),
      Street(8, 9, "123 Main Street Apt 5a"),
      Token.Concatenation(10, 11, List(
        TownName(10, 11, "San Diego"), Token.Terminal(12, 13, ","),
        State(14, 16, "CA"), Token.SP(17),
        ZipCode(17, 18, 92101, Some(1234))
      ))
    )))
    val actual = Parser.parse(address, PostalGrammar.PostalAddress)
    actual should equal(expected)
  }

  it should "parse NamePart" in {
    import PostalAddressTokens._
    val expected = Left(Name(1, 12, "Joe F. Smith III"))
    val actual = Parser.parse("Joe F. Smith III\n", PostalGrammar.NamePart)
    actual should equal(expected)
  }

  it should "parse PersonalPart" in {
    import Token.{ALPHA => A, Terminal => T}
    val token = Token.Concatenation(0, 2, List(A(0), T(1, 2, ".")))
    PostalGrammar.PersonalPart.parse("F.", 0) should equal((2, Left(token)))
  }

  it should "parse Initial" in {
    import Token.{ALPHA => A, Terminal => T}
    val token = Token.Concatenation(0, 2, List(A(0), T(1, 2, ".")))
    PostalGrammar.Initial.parse("F.", 0) should equal((2, Left(token)))
  }

  it should "parse StreetPart" in {
    import PostalAddressTokens._
    val expected = Left(Street(0, 19, "5 123a Main Street"))
    val actual = Parser.parse("5 123a Main Street\n", PostalGrammar.StreetPart)
    actual should equal(expected)
  }

  it should "parse TownName" in {
    import PostalAddressTokens._
    val expected = Left(TownName(0, 8, "San Jose"))
    val actual = Parser.parse("San Jose", PostalGrammar.TownName)

    actual should equal(expected)
  }

  it should "parse State" in {
    import PostalAddressTokens._
    val expected = Left(State(0, 2, "CA"))
    val actual = Parser.parse("CA", PostalGrammar.State)

    actual should equal(expected)
  }

  it should "parse 5 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Left(ZipCode(0, 5, 90210, None))
    val actual = Parser.parse("90210", PostalGrammar.ZipCode)

    actual should equal(expected)
  }

  it should "parse 5+4 digit ZipCode" in {
    import PostalAddressTokens._
    val expected = Left(ZipCode(0, 10, 90210, Some(1234)))
    val actual = Parser.parse("90210-1234", PostalGrammar.ZipCode)

    actual should equal(expected)
  }

  "BacktrackGrammar.a.parse" should "backtrack on failure in alternative" in {
    val text = "x,y."
    val expected = Left(4, Token.Concatenation(0, 4, List(Token.Terminal(0, 3, "x,y"), Token.Terminal(3, 4, "."))))
    BacktrackGrammar.A.parse(text, 0) should equal(expected)
  }
}

object BacktrackGrammar {

  import Rule._

  // a = (b1 / b2) "."
  case object A extends Concatenation(Alternative(B1, B2), Terminal("."))

  // b1 = *ALPHA
  case object B1 extends VariableRepetition(ALPHA)

  // b2 = "x,y"
  case object B2 extends Terminal("x,y")

}

object PostalGrammar {

  import Rule._

  //postal-address   = name-part street zip-part
  case object PostalAddress extends Concatenation(NamePart, StreetPart, ZipPart)

  //name-part        = *(personal-part SP) last-name [SP suffix] newline
  //name-part        =/ personal-part newline
  case object NamePart extends CustomRule3(Alternative(
    Concatenation(
      VariableRepetition(Concatenation(PersonalPart, SP)),
      LastName,
      OptionalSequence(SP, Suffix),
      NewLine
    ),
    Concatenation(PersonalPart, NewLine)
  )) {
    override def intercept(input: String, start: Int, end: Int, token: Token) = token match {
      case Token.Concatenation(_, _, y: List[Token]) =>
        val x = y.reverse.head
        Left(PostalAddressTokens.Name(start, end, input.substring(start, x.start)))
    }
  }

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
  case object StreetPart extends CustomRule3(Concatenation(
    OptionalSequence(Apt, SP),
    HouseNum,
    SP,
    StreetName,
    NewLine
  )) {
    override def intercept(input: String, start: Int, end: Int, token: Token) = token match {
      case Token.Concatenation(_, _, List(_, _, _, _, x)) =>
        Left(PostalAddressTokens.Street(start, end, input.substring(start, x.start)))

    }
  }

  //apt              = 1*4DIGIT
  case object Apt extends VariableRepetition(DIGIT, Some(1), Some(4))

  //house-num        = 1*8(DIGIT / ALPHA)
  case object HouseNum extends VariableRepetition(Alternative(DIGIT, ALPHA), Some(1), Some(8))

  //street-name      = 1*VCHAR
  case object StreetName extends VariableRepetition(Alternative(VCHAR, SP), Some(1))

  //zip-part         = town-name "," SP state 1*2SP zip-code newline
  case object ZipPart extends CustomRule3(Concatenation(
    TownName,
    Terminal(","),
    SP,
    State,
    VariableRepetition(SP, Some(1), Some(2)),
    ZipCode,
    NewLine)) {

    import abnf.{PostalAddressTokens => t}

    override def intercept(input: String, start: Int, end: Int, token: Token) = token match {
      case Token.Concatenation(_, _, List(town: t.TownName, _, _, state: t.State, _, zip: t.ZipCode, x)) =>
        Left(Token.Concatenation(start, end, List(town, state, zip)))
    }
  }

  //town-name        = 1*(ALPHA / SP)
  case object TownName extends CustomRule3(VariableRepetition(Alternative(ALPHA, SP), Some(1))) {
    override def intercept(input: String, start: Int, end: Int, token: Token) =
      Left(PostalAddressTokens.TownName(start, end, input.substring(start, end)))
  }

  //state            = 2ALPHA
  case object State extends CustomRule3(SpecificRepetition(ALPHA, 2)) {
    override def intercept(input: String, start: Int, end: Int, token: Token) =
      Left(PostalAddressTokens.State(start, end, input.substring(start, end)))
  }

  //zip-code         = 5DIGIT ["-" 4DIGIT]
  case object ZipCode extends CustomRule3(Concatenation(
    SpecificRepetition(DIGIT, 5),
    OptionalSequence(Terminal("-"), SpecificRepetition(DIGIT, 4))
  )) {

    import abnf.{Token => t}

    override def intercept(input: String, start: Int, end: Int, token: Token) = token match {
      case t.Concatenation(_, _, List(z1: t.Repetition, t.OptionalSequence(_, _, List(_, z2: t.Repetition)))) =>
        Left(PostalAddressTokens.ZipCode(start, end,
          input.substring(z1.start, z1.end).toInt,
          Some(input.substring(z2.start, z2.end).toInt)))
      case Token.Concatenation(_, _, List(z1: Token.Repetition, _)) =>
        Left(PostalAddressTokens.ZipCode(start, end, input.substring(z1.start, z1.end).toInt, None))
    }
  }

  //newline          = CRLF / LF
  case object NewLine extends CustomRule3(Alternative(CRLF, LF)) {
    override def intercept(input: String, start: Int, end: Int, matched: Token) =
      Left(PostalAddressTokens.NewLine(start, end))
  }

}

object PostalAddressTokens {

  case class NewLine(start: Int, end: Int) extends Token

  case class Name(start: Int, end: Int, value: String) extends Token

  case class Street(start: Int, end: Int, value: String) extends Token

  case class TownName(start: Int, end: Int, value: String) extends Token

  case class State(start: Int, end: Int, code: String) extends Token

  case class ZipCode(start: Int, end: Int, zip: Int, plus4: Option[Int]) extends Token

}
