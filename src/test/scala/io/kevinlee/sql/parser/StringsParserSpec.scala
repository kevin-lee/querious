package io.kevinlee.sql.parser

import eu.timepit.refined.auto._
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import hedgehog._
import hedgehog.runner._


/**
 * @author Kevin Lee
 * @since 2017-07-22
 */
object StringsParserSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      """Parsers.strings.parse("''") should return Success("", 2)""",
      testParsersStringsParseEmptySingleQuotedString
    ),
    property(
      """Parsers.strings.parse("'alphaNumStr'") should return Success(alphaNumStr, alphaNumStr.length + 2)""",
      testParsersStringsParseSingleQuotedAlphaNumericString
    ),
    property(
      """Parsers.strings.parse("'no single quote no backslash unicode String value'") should return Success(the value, the value.length + 2)""",
      testParsersStringsParseSingleQuotedUnicodeString
    ).withTests(1000),
    example(
      """Parsers.strings.parse("''''") should return Success("'", 4)""",
      testParsersStringsParseSingleQuotedStringWithOnlyOneEscapedSingleQuoteInIt
    ),
    example(
      """Parsers.strings.parse("'abc''def'") should return Success("abc'def", "abc'def".length + 3)""",
      testParsersStringsParseSingleQuotedStringWithOneEscapedSingleQuoteInIt
    ),
    example(
      """Parsers.strings.parse("'abc''''def'") should return Success("abc''def", "abc''def".length + 4)""",
      testParsersStringsParseSingleQuotedStringWithTwoSequentialEscapedSingleQuotesInIt
    ),
    example(
      """Parsers.strings.parse("'abc''de''f'") should return Success("abc'de'f", "abc'de'f".length + 4)""",
      testParsersStringsParseSingleQuotedStringWithTwoEscapedSingleQuotesInIt
    )
  )

  def testParsersStringsParseEmptySingleQuotedString: Result = {
    val expected: Success[String, Char, String] = Success("", 2)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("''")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedUnicodeString: Property = for {
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 50))
      .map { s =>
        s.map {
          case '\'' =>
            ((math.random * 10).toInt + '\''.toInt).toChar
          case '\\' =>
            ((math.random * 10).toInt + '\\'.toInt).toChar
          case c =>
            c
        }
      }
      .log("value")
  } yield {
    val expected: Success[String, Char, String] = Success(value, value.length + 2)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("'" + value + "'")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedAlphaNumericString: Property = for {
    value <- Gen.string(Gen.alphaNum, Range.linear(1, 30)).log("value")
  } yield {
    val expected: Success[String, Char, String] = Success(value, value.length + 2)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("'" + value + "'")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedStringWithOnlyOneEscapedSingleQuoteInIt: Result = {
    val expected: Success[String, Char, String] = Success("'", 4)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("''''")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedStringWithOneEscapedSingleQuoteInIt: Result = {
    val expectedString = "abc'def"
    val expected: Success[String, Char, String] = Success(expectedString, expectedString.length + 3)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("'abc''def'")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedStringWithTwoSequentialEscapedSingleQuotesInIt: Result = {
    val expectedString = "abc''def"
    val expected: Success[String, Char, String] = Success(expectedString, expectedString.length + 4)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("'abc''''def'")
    actual ==== expected
  }

  def testParsersStringsParseSingleQuotedStringWithTwoEscapedSingleQuotesInIt: Result = {
    val expectedString = "abc'de'f"
    val expected: Success[String, Char, String] = Success(expectedString, expectedString.length + 4)
    val actual: Parsed[String, Char, String] = Parsers.strings.parse("'abc''de''f'")
    actual ==== expected
  }

}
