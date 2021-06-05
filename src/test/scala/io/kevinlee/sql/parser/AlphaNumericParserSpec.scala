package io.kevinlee.sql.parser

import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2020-12-08
  */
object AlphaNumericParserSpec extends Properties {

  val nonAlphaNumeric = List(' ', '#', '%')

  override def tests: List[Test] = List(
    example(
      """Parsers.alphaNumeric("") should return return Failure(_, 0, _)""",
      testParsersAlphaNumericParseEmptyString
    ),
    property(
      """Parsers.alphaNumerics(digit String) should return Success(parsed value, length)""",
      testParsersAlphaNumericsParseDigitString
    ),
    property(
      """Parsers.alphaNumerics(digit String1 + non-alphaNumric String + digit String2) should return Success(digit String1, digit String1 length)""",
      testParsersAlphaNumericsParseDigitString_NonDigitString_DigitString
    ),
    property(
      """Parsers.alphaNumerics(alphabet lower case char) should return Success(parsed value, length)""",
      testParsersAlphaNumericsParseAlphaLower
    ),
    property(
      """Parsers.alphaNumerics(alphabet upper case char) should return Success(parsed value, length)""",
      testParsersAlphaNumericsParseAlphaUpper
    ),
    property(
      """Parsers.alphaNumerics(alphabet lower + upper + digit String) should return Success(parsed value, length)""",
      testParsersAlphaNumericsParseAlphaNumeric
    ),
    property(
      """Parsers.alphaNumerics(alphabet lower + upper + digit String with non alpha numeric String) should return Success(only first valid alphanumeric String value, length)""",
      testParsersAlphaNumericsParseAlphaNumericWithNonAlphaNumericString
    )
  )

  def testParsersAlphaNumericParseEmptyString: Result = {
    val actual = Parsers.alphaNumerics.parse("")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersAlphaNumericsParseDigitString: Property =
    for {
      value <- Gen.string(Gen.digit, Range.linear(1, 10)).log("value")
    } yield {
      val expected: Parsed[String, Char, String] = Success(value, value.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(value)
      actual ==== expected
    }

  def testParsersAlphaNumericsParseDigitString_NonDigitString_DigitString
    : Property =
    for {
      value1 <- Gen.string(Gen.digit, Range.linear(1, 10)).log("value1")
      value2 <- Gen.string(Gen.digit, Range.linear(1, 10)).log("value2")
    } yield {
      val expected: Parsed[String, Char, String] =
        Success(value1, value1.length)
      val input = s"$value1${nonAlphaNumeric.mkString}$value2"
      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(input)
      (actual ==== expected).log(s"input: $input")
    }

  def testParsersAlphaNumericsParseAlphaLower: Property =
    for {
      value <- Gen.string(Gen.lower, Range.linear(1, 10)).log("value")
    } yield {
      val expected: Success[String, Char, String] = Success(value, value.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(value)
      actual ==== expected
    }

  def testParsersAlphaNumericsParseAlphaUpper: Property =
    for {
      value <- Gen.string(Gen.upper, Range.linear(1, 10)).log("value")
    } yield {
      val expected: Success[String, Char, String] = Success(value, value.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(value)
      actual ==== expected
    }

  def testParsersAlphaNumericsParseAlphaNumeric: Property =
    for {
      value <- Gen.string(Gen.alphaNum, Range.linear(1, 10)).log("value")
    } yield {
      val expected: Parsed[String, Char, String] = Success(value, value.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(value)
      actual ==== expected
    }

  def testParsersAlphaNumericsParseAlphaNumericWithNonAlphaNumericString
    : Property =
    for {
      value <- Gen
        .string(
          Gen.frequency1(
            7 -> Gen.alphaNum,
            3 -> Gen.elementUnsafe(nonAlphaNumeric)
          ),
          Range.linear(1, 10)
        )
        .log("value")
    } yield {
      val alphaNumericTaken = value.takeWhile(!nonAlphaNumeric.contains(_))

      val actual: Parsed[String, Char, String] =
        Parsers.alphaNumerics.parse(value)
      if (alphaNumericTaken.isEmpty) {
        actual matchPattern { case Failure(_, 0, _) => }
      } else {
        val expected: Parsed[String, Char, String] =
          Success(alphaNumericTaken, alphaNumericTaken.length)
        actual ==== expected

      }
    }

}
